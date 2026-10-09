# Wi-Fi for hosts that boot from a self-contained SD image and have no
# other way onto the network (pitchen, nidavellir).
#
# The repo is public and the card must come up on its own, so SSID and
# passphrase are not in the image: they are read from `wifi.env` on the
# firmware partition, dropped there after flashing:
#
#   WIFI_SSID="the network name"
#   WIFI_PSK="the passphrase"
{lib, ...}: {
  networking.networkmanager = {
    enable = true;
    wifi.powersave = false;
    ensureProfiles = {
      environmentFiles = ["/boot/firmware/wifi.env"];
      profiles.home = {
        connection = {
          id = "home";
          type = "wifi";
          autoconnect = true;
        };
        wifi = {
          mode = "infrastructure";
          ssid = "$WIFI_SSID";
        };
        wifi-security = {
          key-mgmt = "wpa-psk";
          psk = "$WIFI_PSK";
        };
        ipv4.method = "auto";
        ipv6.method = "auto";
      };
    };
  };
  # /boot/firmware is an automount; have it mounted before systemd reads
  # the EnvironmentFile from it.
  systemd.services.NetworkManager-ensure-profiles.unitConfig.RequiresMountsFor = "/boot/firmware";
  boot.kernelParams = ["cfg80211.ieee80211_regdom=ES"];

  # The credentials sit on the firmware partition, so keep it root-only.
  # Same options nixos-raspberrypi's sd-image module forces, plus the umask.
  fileSystems."/boot/firmware".options = lib.mkOverride 40 [
    "noatime"
    "noauto"
    "x-systemd.automount"
    "x-systemd.idle-timeout=1min"
    "umask=0077"
  ];

  # <hostname>.local, so the host is reachable without knowing its lease.
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };
}
