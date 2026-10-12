# Wi-Fi for hosts that boot from a self-contained SD image and have no
# other way onto the network (nidavellir, pitchen).
#
# Wi-Fi has to come up unattended on the very first boot — before there
# is any way to log in and before a host key exists for sops. The
# passphrase therefore travels on the FAT firmware partition, dropped
# there right after flashing (scripts/flash-sd.sh), and never enters the
# repo or the nix store.
{pkgs, ...}: let
  wifiSecrets = "/var/lib/wifi-secrets";
in {
  networking.wireless = {
    enable = true;
    secretsFile = wifiSecrets;
    networks."Furgoneta de cacharrancia" = {
      pskRaw = "ext:psk_furgoneta";
      # The SSID is not broadcast: without a directed probe it never
      # shows up in a scan and wpa_supplicant scans forever.
      hidden = true;
    };
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
}
