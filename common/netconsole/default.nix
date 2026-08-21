{pkgs, ...}: let
  # hulk -> nuc-3. Both sit on 10.0.4.0/24 (enp36s0f1 here, enp2s0 there), so
  # this is a single L2 hop and netconsole never needs a gateway.
  srcDev = "enp36s0f1";
  srcPort = "6665";
  dstIp = "10.0.4.32";
  dstPort = "6666";

  # Fallback only. The unit reads nuc-3's MAC out of the neighbour table at
  # start and uses this literal only if that fails. The MAC has to be baked
  # into the target up front either way: netconsole transmits from contexts
  # where the kernel cannot ARP -- that is the entire point of it.
  dstMacFallback = "88:ae:dd:0c:05:0a";

  targetDir = "/sys/kernel/config/netconsole/nuc-3";

  up = pkgs.writeShellScript "netconsole-target-up" ''
    set -eu
    PATH=${pkgs.iproute2}/bin:${pkgs.iputils}/bin:${pkgs.gawk}/bin:$PATH

    src_ip=$(ip -4 -o addr show dev ${srcDev} | awk '{print $4}' | cut -d/ -f1 | head -n1)
    if [ -z "''${src_ip}" ]; then
      echo "no IPv4 address on ${srcDev} yet, will retry" >&2
      exit 1
    fi

    # Prime the neighbour table so we can use the MAC nuc-3 actually has today
    # rather than trusting a literal that goes stale the day its NIC changes.
    ping -c1 -W2 ${dstIp} >/dev/null 2>&1 || true
    dst_mac=$(ip neigh show ${dstIp} dev ${srcDev} | awk '/lladdr/ {print $3; exit}')
    if [ -z "''${dst_mac}" ]; then
      dst_mac=${dstMacFallback}
      echo "could not resolve ${dstIp}, falling back to ''${dst_mac}" >&2
    fi

    mkdir -p ${targetDir}
    # A target must be disabled before any of its attributes can be written.
    echo 0 > ${targetDir}/enabled 2>/dev/null || true
    echo ${srcDev}       > ${targetDir}/dev_name
    echo "''${src_ip}"   > ${targetDir}/local_ip
    echo ${srcPort}      > ${targetDir}/local_port
    echo ${dstIp}        > ${targetDir}/remote_ip
    echo ${dstPort}      > ${targetDir}/remote_port
    echo "''${dst_mac}"  > ${targetDir}/remote_mac
    echo 1               > ${targetDir}/enabled

    echo "netconsole: ''${src_ip}:${srcPort} -> ${dstIp}:${dstPort} (''${dst_mac}) via ${srcDev}"
  '';

  down = pkgs.writeShellScript "netconsole-target-down" ''
    set -eu
    [ -d ${targetDir} ] || exit 0
    echo 0 > ${targetDir}/enabled 2>/dev/null || true
    rmdir ${targetDir} 2>/dev/null || true
  '';
in {
  # Get kernel output off this box before it dies.
  #
  # hulk has now hung four times (2026-07-11, 2026-08-07, 2026-08-08,
  # 2026-08-20) leaving *nothing* on disk: journal stops mid-line, pstore
  # empty, BMC SEL empty. Every hypothesis about those hangs is currently
  # unfalsifiable because the machine cannot tell us anything while it dies.
  # netconsole is the only channel that does not depend on hulk surviving long
  # enough to write to its own storage.
  #
  # Be clear about what this can and cannot do. It ships whatever the kernel
  # *prints*, as it prints it. If hang #5 produces an oops, a panic, an RCU
  # stall or a hard-lockup backtrace, nuc-3 gets it. If the box goes silent
  # without printing anything -- which is what all four hangs look like so far
  # -- netconsole will also show nothing.
  #
  # That negative is still worth having. Right now we cannot distinguish "the
  # kernel printed a diagnostic and could not write it to disk" from "the
  # kernel never printed anything at all". Those point at very different
  # failure modes, and this settles which one we are in.
  boot.kernelModules = ["netconsole"];

  # netconsole is a console driver, so it only ever sees messages that pass
  # console_loglevel. NixOS defaults to 4 (err and above), which drops
  # KERN_WARNING -- and warnings are where hung-task and several stall
  # reports live. 6 includes warnings and info.
  #
  # Cost: a chattier VT on hulk. Drop this line back to the default if that
  # becomes annoying, at the price of a thinner capture.
  boot.consoleLogLevel = 6;

  systemd.services.netconsole-target = {
    description = "Point netconsole at nuc-3";
    wants = ["network-online.target"];
    after = ["network-online.target" "systemd-modules-load.service"];
    wantedBy = ["multi-user.target"];

    # The link can take a while to carry traffic after network-online fires;
    # keep retrying rather than giving up on the first miss.
    unitConfig.StartLimitIntervalSec = 0;

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${up}";
      ExecStop = "${down}";
      Restart = "on-failure";
      RestartSec = 10;
    };
  };
}
