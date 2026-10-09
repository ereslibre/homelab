# nidavellir — the forge. Raspberry Pi 4 B driving the Ender 3 over USB
# serial with OctoPrint, plus an optional USB webcam. Boots from a
# self-contained SD image (see ./README.md); the web UI is reachable over
# tailscale only.
{
  config,
  lib,
  ...
}: let
  webcamPort = 8080;
in {
  imports = [
    ./hardware-configuration.nix
    ../common/aliases
    ../common/nix
    ../common/packages
    ../common/programs
    ../common/services
    ../common/tailscale
    ../common/users
  ];

  networking.hostName = "nidavellir";

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Madrid";

  # LAN only gets SSH (opened by the openssh module). Everything else —
  # the OctoPrint UI on :80 — is reachable through the tailnet.
  networking.firewall.trustedInterfaces = ["tailscale0"];

  services.octoprint = {
    enable = true;
    host = "127.0.0.1";
    extraConfig = {
      # Printer is a stock-Marlin Ender 3: CH340 USB serial at 115200.
      serial = {
        port = "AUTO";
        baudrate = 115200;
        autoconnect = true;
      };
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
      "--resolution=1280x720"
      "--desired-fps=15"
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
