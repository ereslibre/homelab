# pitchen (kitchen kiosk)

`pitchen` is a Raspberry Pi 5 behind a 10.1" HDMI touchscreen (1280 × 800,
USB touch) that shows Home Assistant full screen. Like `nidavellir` it
boots from a plain SD card with no installer step; unlike it, it is a
Pi 5 on Wi-Fi, which changes two things: where the kernel comes from and
how the card learns the Wi-Fi credentials.

## How the SD card "installs" itself

`just sd-image pitchen` builds an image that already *is* the installed
system (firmware partition + ext4 root carrying the full closure). On the
very first boot NixOS:

1. grows the root partition and filesystem to fill the card,
2. registers the pre-seeded store paths in the nix database,
3. generates the SSH host keys,
4. reads `wifi.env` from the firmware partition and joins the Wi-Fi,

and from then on every boot is a normal boot of the installed system.
Later changes are regular `nixos-rebuild` generations on that same card.

## Bring-up

```sh
# On hulk (binfmt aarch64 emulation)
just sd-image pitchen
zstd -d --stdout result/sd-image/*.img.zst | sudo dd of=/dev/sdX bs=4M status=progress conv=fsync
```

Then drop the Wi-Fi credentials on the firmware partition (the first,
FAT one). They are not in the image because this repository is public:

```sh
sudo mount /dev/sdX1 /mnt
sudo tee /mnt/wifi.env >/dev/null <<'EOF'
WIFI_SSID="the network name"
WIFI_PSK="the passphrase"
EOF
sudo umount /mnt
```

Quote both values (the file is a systemd `EnvironmentFile`). On the
running system the partition is mounted root-only at `/boot/firmware`.

Insert the card and power on. First boot takes a couple of minutes
(partition resize). The screen stays on a console until Home Assistant
answers, then Chromium takes over.

```sh
ssh ereslibre@pitchen.local
```

Home Assistant asks for a login the first time: plug in a USB keyboard
for that one visit, there is no on-screen keyboard. The session then
lives in `/home/kiosk/.config/chromium` on the card.

The EEPROM must have SD in its `BOOT_ORDER`; this Pi has `0xf21`, which
tries the SD card first.

## What's on it

| Piece | Where | Notes |
|---|---|---|
| cage | `cage-tty1.service` | Wayland kiosk compositor, runs as user `kiosk`, restarts with Chromium |
| Chromium | inside cage | `--kiosk` on `/dashboard-kitchen/0`, waits for Home Assistant before starting |
| NetworkManager | `wlan0` | one profile, `home`, filled in from `/boot/firmware/wifi.env` |
| avahi | mDNS | publishes `pitchen.local` |
| tailscale | `tailscale0` | optional, `sudo tailscale up` once if wanted |

## Touch

Finger drag scrolls and pinch zooms, as on a tablet. This needs nothing
beyond what is here: cage hands touches to Chromium as touch events. The
Raspberry Pi OS install this replaces had labwc turn every touch into a
mouse pointer (`mouseEmulation="yes"`), which is why scrolling meant
hunting for scrollbars.

## The kernel is built locally

A Pi 5 needs the Raspberry Pi vendor kernel and firmware, which come from
the [`nixos-raspberrypi`](https://github.com/nvmd/nixos-raspberrypi) flake
rather than `nixos-hardware`. The repo follows nixos-unstable, so the
input tracks that flake's `nixos-unstable` branch, and its binary cache
only carries kernels for the stable branches. Expect the first
`just sd-image pitchen`, and any later one after bumping that input, to
compile the kernel: long under emulation on hulk, much quicker on a
native aarch64 builder.

## Updating

Same flow as `nidavellir` — build on hulk, push, activate:

```sh
just build pitchen
TOPLEVEL=$(readlink -f ./result)
nix copy --to ssh-ng://ereslibre@pitchen.local "$TOPLEVEL"
ssh -t ereslibre@pitchen.local "sudo nix-env -p /nix/var/nix/profiles/system --set $TOPLEVEL && sudo $TOPLEVEL/bin/switch-to-configuration switch"
```

A change that restarts `cage-tty1.service` blanks the screen for a few
seconds.

## Secrets

None in the repo, so sops-nix is not wired in; the Wi-Fi credentials live
only on the card. If the host grows secrets, follow the same
post-first-boot rekey as the other machines (`just age-gen pitchen`, add
`&host-pitchen` to `.sops.yaml`).
