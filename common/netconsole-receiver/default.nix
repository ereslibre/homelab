{pkgs, ...}: {
  # Receiving end of hulk's netconsole (see ../netconsole).
  #
  # hulk sends UDP datagrams containing raw kernel log lines to this host on
  # 6666. socat reads them off the socket and writes them to stdout; systemd
  # puts stdout in the journal. So `journalctl -u netconsole-receiver` on
  # nuc-3 is a live tail of hulk's kernel ring, and -- crucially -- it keeps
  # whatever hulk managed to print in the seconds before it stopped being able
  # to write to its own disk.
  #
  # The kernel's own monotonic timestamps travel inside the messages, which is
  # what you actually want for lining a capture up against hulk's journal.
  # journald's receive timestamp on each line is a second, independent clock.
  systemd.services.netconsole-receiver = {
    description = "Collect netconsole datagrams from hulk into the journal";
    wants = ["network-online.target"];
    after = ["network-online.target"];
    wantedBy = ["multi-user.target"];

    serviceConfig = {
      # -u is unidirectional (receive only). No fork: one long-lived process
      # drains every datagram, rather than spawning one per packet -- which
      # matters precisely when hulk is dumping a panic at us.
      ExecStart = "${pkgs.socat}/bin/socat -u UDP-RECV:6666,reuseaddr -";

      Restart = "always";
      RestartSec = 5;

      StandardOutput = "journal";
      StandardError = "journal";
      SyslogIdentifier = "netconsole-hulk";

      # A panic dump arrives as a burst of hundreds of lines in well under a
      # second. journald's default rate limit would throw away exactly the
      # part we care about, so turn it off for this unit only.
      LogRateLimitIntervalSec = 0;

      DynamicUser = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = ["AF_INET" "AF_INET6"];
      SystemCallFilter = ["@system-service"];
    };
  };

  # No firewall rule needed: ../node sets networking.firewall.enable = false.
  # If that ever changes, this needs UDP 6666 opened.
}
