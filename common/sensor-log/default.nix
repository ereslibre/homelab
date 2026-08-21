{pkgs, ...}: let
  intervalSeconds = 30;

  sample = pkgs.writeShellScript "sensor-log" ''
    set -u
    PATH=${pkgs.coreutils}/bin

    while :; do
      line=""
      for h in /sys/class/hwmon/hwmon*; do
        [ -r "$h/name" ] || continue
        chip=$(cat "$h/name")

        case "$chip" in
          k10temp | jc42 | nvme | nct6798) ;;
          *) continue ;;
        esac

        # hwmon numbering is not stable across boots, so key every reading by
        # the chip name plus the bus address of the underlying device instead
        # of by hwmonN. The jc42 sensors have no labels, and their i2c address
        # is the only thing that tells one DIMM apart from another.
        dev=$(basename "$(readlink -f "$h/device" 2>/dev/null || echo unknown)")

        for t in "$h"/temp*_input; do
          [ -r "$t" ] || continue
          base=''${t%_input}

          if [ -r "''${base}_label" ]; then
            label=$(cat "''${base}_label")
          else
            label=$(basename "$base")
          fi

          case "$chip" in
            # One number per drive is enough; Sensor 1/2 track Composite.
            nvme)
              [ "$label" = "Composite" ] || continue
              ;;
            # SYSTIN is the closest thing this board has to a room-ambient
            # reading, and CPUTIN is a useful cross-check on k10temp.
            # Everything else on the nct6798 is untrustworthy: AUXTIN0-4 read
            # 74-84 C on an idle machine and the chip alarms on nearly every
            # voltage rail against a max of 0.00 V, which is what an
            # unconfigured sensors.conf looks like rather than a real fault.
            nct6798)
              case "$label" in
                SYSTIN | CPUTIN) ;;
                *) continue ;;
              esac
              ;;
          esac

          milli=$(cat "$t") || continue
          label=''${label// /_}
          line="$line $chip/$dev/$label=$((milli / 1000)).$(((milli % 1000) / 100))"
        done
      done

      echo "temps:$line"
      sleep ${toString intervalSeconds}
    done
  '';
in {
  # Continuous temperature trail, so the next unexplained hang arrives with
  # thermal context attached instead of nothing.
  #
  # Why this exists: hulk's failing DIMM and its four silent hangs both have a
  # plausible temperature component -- DRAM leakage roughly doubles per 10 C,
  # so refresh margin shrinks as a DIMM heats, and three of the four hangs
  # landed in the hot part of an August afternoon. That is suggestive at n=4
  # and nothing more, and it will stay unfalsifiable until somebody is
  # actually recording the numbers. Nothing in this repo was.
  #
  # The box has four live jc42 DIMM sensors on the SMBus that nobody reads.
  # Only 4 of the 8 DIMMs enumerate a TSOD; the rest are presumably behind a
  # second SMBus segment or colliding on address.
  #
  # Caveat: this writes to the journal, and the journal is precisely what dies
  # at the cut. The last sample before a hang will be up to
  # ${toString intervalSeconds}s stale. ../netconsole is what gets data off
  # the box; this is what gives the on-disk record something to say.
  #
  # Not covered: GPU temperatures, which need nvidia-smi and a process spawn
  # per sample. Add them here if the GPUs ever become a suspect.
  systemd.services.sensor-log = {
    description = "Log DIMM, CPU, NVMe and board temperatures to the journal";
    after = ["multi-user.target"];
    wantedBy = ["multi-user.target"];

    serviceConfig = {
      ExecStart = "${sample}";
      Restart = "always";
      RestartSec = 10;

      SyslogIdentifier = "sensor-log";

      DynamicUser = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = [];
      SystemCallFilter = ["@system-service"];

      # One short line every ${toString intervalSeconds}s is well inside
      # journald's default rate limit, but this unit is deliberately chatty
      # forever -- make sure it can never be the thing that gets throttled.
      LogRateLimitIntervalSec = 0;
    };
  };
}
