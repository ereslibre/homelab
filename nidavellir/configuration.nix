# nidavellir — the forge. Raspberry Pi Zero 2 W driving the Ender 5 S1
# over USB serial with OctoPrint, plus an optional USB webcam. Boots from
# a self-contained SD image and joins Wi-Fi on its own (see ./README.md);
# the web UI is served on the LAN.
{
  config,
  lib,
  pkgs,
  ...
}: let
  webcamPort = 8080;
  wifiSecrets = config.networking.wireless.secretsFile;

  # Stand-in for `vcgencmd get_throttled`, which OctoPrint's Pi support
  # plugin polls: vcgencmd talks to the firmware through /dev/vcio, and
  # only the Pi vendor kernel has that. Mainline reports under-voltage
  # through hwmon instead, so answer in vcgencmd's format from there
  # (bit 0 is "under-voltage now"). Frequency capping and thermal
  # throttling are not exposed, so those are never flagged.
  throttleCheck = pkgs.writeShellScript "get-throttled" ''
    for name in /sys/class/hwmon/hwmon*/name; do
      if [ "$(${pkgs.coreutils}/bin/cat "$name")" = rpi_volt ]; then
        echo "throttled=0x$(${pkgs.coreutils}/bin/cat "''${name%name}in0_lcrit_alarm")"
        exit 0
      fi
    done
    echo "no rpi_volt hwmon device" >&2
    exit 1
  '';
in {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/nix
    ../common/packages
    ../common/programs
    ../common/services
    ../common/users
    ../common/wifi
  ];

  networking.hostName = "nidavellir";

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Madrid";

  # SSH (opened by the openssh module) and the OctoPrint UI on :80, both
  # on the LAN. The host is not on the tailnet: from outside, the network
  # is reached through nuc-1.
  networking.firewall.allowedTCPPorts = [80];

  # The board is headless and Wi-Fi is its only way in, so when Wi-Fi does
  # not come up there is nothing to log into. Two ways out, neither of
  # which needs a keyboard:
  #
  # 1. A serial console over the data socket: with a plain micro-USB
  #    cable to a computer (no OTG adapter) the port is a USB device and
  #    shows up there as a serial port with a login prompt. With the OTG
  #    adapter in, it is a host as usual and this stays idle.
  boot.kernelModules = ["g_serial" "raspberrypi-hwmon"];
  systemd.services."serial-getty@ttyGS0" = {
    overrideStrategy = "asDropin";
    wantedBy = ["getty.target"];
  };

  # 2. A bring-up report on the FAT firmware partition, readable from any
  #    computer the card is moved to. Rewritten a few times, as
  #    association can take a while; the passphrase itself is never
  #    written to it.
  systemd.services.boot-report = {
    description = "Write a network bring-up report to the firmware partition";
    wantedBy = ["multi-user.target"];
    after = ["wifi-secrets.service"];
    unitConfig.RequiresMountsFor = "/boot/firmware";
    path = with pkgs; [gawk iproute2 iw util-linux wpa_supplicant];
    script = ''
      set +e
      report() {
        echo "### $(date) — up $(cut -d' ' -f1 /proc/uptime)s — $(uname -r)"
        tr -d '\0' </proc/device-tree/model; echo
        echo "### interfaces"
        ip -br link
        ip -br addr
        rfkill
        echo "### secrets file"
        for f in /boot/firmware/wifi-secrets ${wifiSecrets}; do
          ls -la "$f"
          awk -F= '{ printf "  line %d: key=%s value_chars=%d\n", NR, $1, length($0) - length($1) - 1 }' "$f"
          echo "  carriage returns: $(grep -c $'\r' "$f")"
        done
        echo "### units"
        systemctl --failed --no-pager
        systemctl status --no-pager -l wifi-secrets wpa_supplicant dhcpcd
        echo "### wpa_supplicant"
        for dev in /sys/class/net/*/wireless; do
          dev=$(basename "$(dirname "$dev")")
          wpa_cli -i "$dev" status
          wpa_cli -i "$dev" scan_results
          iw dev "$dev" link
          iw dev "$dev" scan | grep -E '^BSS|SSID:|freq:|signal:|RSN:|WPA:|Authentication suites|Capabilities:.*MFP'
        done
        echo "### kernel"
        dmesg | grep -i -E 'brcm|sdio|mmc1|cfg80211|firmware|wlan|dwc2|gadget'
        echo "### journal"
        journalctl -b --no-pager -u wifi-secrets -u wpa_supplicant -u dhcpcd
        journalctl -b --no-pager -p warning | tail -n 80
      }
      for delay in 45 60 120; do
        sleep "$delay"
        report >/boot/firmware/boot-report.txt 2>&1
        sync
      done
    '';
  };

  # nidavellir.local, so the host is reachable without knowing its lease.
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.octoprint = {
    enable = true;
    host = "127.0.0.1";
    extraConfig = {
      # Printer is a stock-Marlin Ender 5 S1: CH340 USB serial (the USB-C
      # port on the front) at 115200.
      serial = {
        port = "AUTO";
        baudrate = 115200;
        autoconnect = true;
      };
      plugins.pi_support.vcgencmd_throttle_check_command = "${throttleCheck}";
      plugins.classicwebcam = {
        stream = "/webcam/stream";
        snapshot = "http://127.0.0.1:${toString webcamPort}/snapshot";
      };
      # Power menu in the UI, backed by the sudo rules below. The box is
      # headless and remote, so this is the only convenient way to bounce it.
      server.commands = {
        serverRestartCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl restart octoprint.service";
        systemRestartCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl reboot";
        systemShutdownCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl poweroff";
      };
    };
  };

  security.sudo.extraRules = [
    {
      users = [config.services.octoprint.user];
      commands =
        map (command: {
          command = "/run/current-system/sw/bin/systemctl ${command}";
          options = ["NOPASSWD"];
        }) [
          "restart octoprint.service"
          "reboot"
          "poweroff"
        ];
    }
  ];

  # Webcam is optional and hot-pluggable. The udev rule gives the first
  # capture node of any USB camera a stable name (the Pi's own
  # bcm2835-codec nodes also show up as /dev/video*, so /dev/video0 is
  # not reliable) and starts the streamer when it appears; BindsTo stops
  # it again on unplug instead of leaving it in a restart loop.
  services.udev.extraRules = ''
    SUBSYSTEM=="video4linux", ENV{ID_BUS}=="usb", ATTR{index}=="0", SYMLINK+="webcam", TAG+="systemd", ENV{SYSTEMD_WANTS}+="ustreamer.service"
  '';

  services.ustreamer = {
    enable = true;
    device = "/dev/webcam";
    listenAddress = "127.0.0.1:${toString webcamPort}";
    extraArgs = [
      "--format=MJPEG"
      # Kept modest: the Zero 2 W shares four small cores and 512 MB
      # between this and OctoPrint.
      "--resolution=640x480"
      "--desired-fps=10"
    ];
  };

  systemd.services.ustreamer = {
    wantedBy = lib.mkForce [];
    bindsTo = ["dev-webcam.device"];
    after = ["dev-webcam.device"];
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    # G-code uploads.
    clientMaxBodySize = "1g";
    virtualHosts."nidavellir" = {
      default = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.octoprint.port}";
        proxyWebsockets = true;
      };
      locations."/webcam/" = {
        proxyPass = "http://127.0.0.1:${toString webcamPort}/";
        extraConfig = ''
          proxy_buffering off;
        '';
      };
    };
  };

  system.stateVersion = "26.11";
}
