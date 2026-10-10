# nidavellir — the forge. Raspberry Pi Zero 2 W driving the Ender 5 S1
# over USB serial with OctoPrint, plus an optional USB webcam. Boots from
# a self-contained SD image and joins Wi-Fi on its own (see ./README.md);
# the web UI is reachable over tailscale only.
{
  config,
  lib,
  pkgs,
  ...
}: let
  webcamPort = 8080;
  wifiSecrets = "/var/lib/wifi-secrets";
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

  # The Zero 2 W has no Ethernet, so Wi-Fi has to come up unattended on
  # the very first boot — before there is any way to log in and before a
  # host key exists for sops. The passphrase therefore travels on the FAT
  # firmware partition, dropped there right after flashing (see
  # ./README.md), and never enters the repo or the nix store.
  networking.wireless = {
    enable = true;
    secretsFile = wifiSecrets;
    networks."Furgoneta de cacharrancia".pskRaw = "ext:psk_furgoneta";
    extraConfig = ''
      country=ES
    '';
  };

  # wpa_supplicant runs unprivileged and cannot read a root-only FAT
  # mount, so hand it a private copy. Re-copied on every boot: to change
  # the passphrase, edit the file on the card from any computer.
  systemd.services.wifi-secrets = {
    description = "Install Wi-Fi secrets from the firmware partition";
    before = ["wpa_supplicant.service"];
    requiredBy = ["wpa_supplicant.service"];
    unitConfig.RequiresMountsFor = "/boot/firmware";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      if [ -f /boot/firmware/wifi-secrets ]; then
        ${pkgs.coreutils}/bin/install -o wpa_supplicant -g wpa_supplicant -m 0400 \
          /boot/firmware/wifi-secrets ${wifiSecrets}
      fi
      [ -f ${wifiSecrets} ]
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
