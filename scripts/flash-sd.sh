#!/usr/bin/env bash
# Write a host's self-contained SD image to a card and seed its Wi-Fi
# passphrase.
#
# The passphrase is kept in <host>/secrets.yaml (sops, key `wifi_psk`),
# encrypted to the user key only. It cannot be decrypted on the Pi — a
# Wi-Fi-only board needs it before it has a host key, or any network to
# be rekeyed over — so it is decrypted here and written to the card's
# FAT firmware partition as `wifi-secrets`, where the host picks it up
# on boot. It never enters the image or the nix store.
#
# Usage:
#   scripts/flash-sd.sh <hostname> <device>      # e.g. nidavellir /dev/sdX
#
# Expects ./result to be the host's sdImage (`just flash-sd` builds it).

set -euo pipefail

HOST="${1:?usage: flash-sd.sh <hostname> <device>}"
DEVICE="${2:?usage: flash-sd.sh <hostname> <device>}"
SECRETS="$HOST/secrets.yaml"
PLACEHOLDER="CHANGEME"
export SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-$HOME/.config/sops/age/keys.txt}"

die() {
  echo "error: $*" >&2
  exit 1
}

IMAGE=$(echo result/sd-image/*.img.zst)
[ -f "$IMAGE" ] || die "no image under result/sd-image/ — run: just sd-image $HOST"
[ -b "$DEVICE" ] || die "$DEVICE is not a block device"
[ "$(lsblk -dno TYPE "$DEVICE")" = "disk" ] || die "$DEVICE is not a whole disk"
# Only cards and sticks: never an internal disk picked by a typo.
case "$(lsblk -dno RM,TRAN "$DEVICE" | tr -s ' ')" in
  1\ * | *usb | *mmc) ;;
  *) die "$DEVICE is not removable, refusing to overwrite it" ;;
esac
if lsblk -no MOUNTPOINTS "$DEVICE" | grep -q .; then
  die "$DEVICE has mounted partitions, unmount them first"
fi

# Decrypt before touching the card, so a missing key fails early.
PSK=$(nix develop --accept-flake-config --command sops decrypt --extract '["wifi_psk"]' "$SECRETS")
[ -n "$PSK" ] && [ "$PSK" != "$PLACEHOLDER" ] ||
  die "wifi_psk is not set in $SECRETS — run: just edit-secrets $HOST"

echo "Writing $IMAGE to $DEVICE ($(lsblk -dno SIZE,MODEL "$DEVICE" | tr -s ' '))"
zstd -d --stdout "$IMAGE" | sudo dd of="$DEVICE" bs=4M status=progress conv=fsync
sudo blockdev --rereadpt "$DEVICE"
sudo udevadm settle

FIRMWARE=$(lsblk -lnpo NAME "$DEVICE" | sed -n 2p)
[ -b "$FIRMWARE" ] || die "cannot find the firmware partition on $DEVICE"

MNT=$(mktemp -d)
trap 'sudo umount "$MNT" 2>/dev/null || true; rmdir "$MNT"' EXIT
sudo mount "$FIRMWARE" "$MNT"
printf 'psk_furgoneta=%s\n' "$PSK" | sudo tee "$MNT/wifi-secrets" >/dev/null
sudo umount "$MNT"
sync

echo "Done. Move the card to $HOST and power it on."
