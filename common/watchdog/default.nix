{...}: {
  # Recovery path for machines that hang hard enough to stop writing logs.
  #
  # hulk has done this twice with no trace on disk (2026-07-11 03:15 and
  # 2026-08-07 18:34): the journal stops mid-line, pstore stays empty because no
  # kernel code runs far enough to write an ERST record, and the box sits dead
  # until someone issues an IPMI reset by hand -- 33 minutes of downtime on
  # 2026-08-07. The hardware watchdog is the only recovery path here that does
  # not depend on the kernel still being able to schedule anything.
  #
  # systemd arms /dev/watchdog0 from PID 1 and pings it every
  # RuntimeWatchdogSec/2. If PID 1 stops getting scheduled, the board resets
  # itself. 60s is well under the SP5100 TCO's 65535s ceiling and long enough
  # that a loaded nix build stalling PID 1 briefly will not trip it.
  systemd.settings.Manager.RuntimeWatchdogSec = "60s";

  # kernel.panic defaults to 0, meaning a panic halts forever. That is how the
  # 2026-07-09 fatal machine check on hulk turned into hours of downtime instead
  # of a reboot. 30s leaves the panic path time to hand its dmesg record to
  # pstore/ERST (which worked on 2026-07-09) before the box comes back.
  #
  # Deliberately NOT setting panic_on_oops or hardlockup_panic: the watchdog
  # above already covers the wedge case, and it covers strictly more of it --
  # including hangs where the NMI watchdog itself never gets to run. Converting
  # every oops into a reboot would just take an interactive workstation down
  # over faults it can currently survive.
  boot.kernel.sysctl."kernel.panic" = 30;
}
