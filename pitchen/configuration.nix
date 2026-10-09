# pitchen — the kitchen Pi. Raspberry Pi 5 behind a 10.1" HDMI touchscreen
# (1280x800, USB touch) that shows Home Assistant full screen. Boots from a
# self-contained SD image (see ./README.md) and joins the Wi-Fi with
# credentials read from the firmware partition (../common/firmware-wifi).
{
  lib,
  pkgs,
  ...
}: let
  url = "https://home-assistant.ereslibre.net/dashboard-kitchen/0";

  # Chromium has no auto-retry on a failed first load, and at boot the
  # Wi-Fi is usually not up yet: wait for Home Assistant before starting.
  kiosk = pkgs.writeShellScript "pitchen-kiosk" ''
    for _ in $(seq 60); do
      ${lib.getExe pkgs.curl} --silent --fail --max-time 5 --output /dev/null \
        https://home-assistant.ereslibre.net/manifest.json && break
      sleep 2
    done
    exec ${lib.getExe pkgs.chromium} \
      --kiosk \
      --ozone-platform=wayland \
      --touch-events=enabled \
      --enable-pinch \
      --enable-features=OverlayScrollbar \
      --no-first-run \
      --noerrdialogs \
      --disable-infobars \
      --disable-session-crashed-bubble \
      --hide-crash-restore-bubble \
      --disable-features=Translate \
      --password-store=basic \
      ${lib.escapeShellArg url}
  '';
in {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/firmware-wifi
    ../common/nix
    ../common/packages
    ../common/programs
    ../common/services
    ../common/tailscale
    ../common/users
  ];

  networking.hostName = "pitchen";

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Madrid";

  # The kiosk: cage runs one Wayland client full screen on tty1, with no
  # desktop around it. Touches reach Chromium as touch events (cage does no
  # mouse emulation), which is what makes drag-to-scroll and pinch work.
  users.users.kiosk = {
    isNormalUser = true;
    uid = 1100;
    extraGroups = ["video" "input"];
  };

  services.cage = {
    enable = true;
    user = "kiosk";
    program = kiosk;
  };
  # cage exits with its client; bring both back if Chromium ever dies.
  systemd.services.cage-tty1.serviceConfig = {
    Restart = "always";
    RestartSec = 5;
  };

  system.stateVersion = "26.11";
}
