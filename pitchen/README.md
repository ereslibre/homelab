# pitchen (kitchen kiosk)

`pitchen` is a Raspberry Pi 5 behind a 10.1" HDMI touchscreen (1280 × 800,
USB touch) that shows Home Assistant full screen. Like `nidavellir` it
boots from a plain SD card with no installer step and joins the Wi-Fi on
its own; unlike it, it is a Pi 5 driving a screen, which is why it runs a
newer kernel (see [Kernel](#kernel)).

## How the SD card "installs" itself

`just sd-image pitchen` builds an image that already *is* the installed
system (firmware partition + ext4 root carrying the full closure). On the
very first boot NixOS:

1. grows the root partition and filesystem to fill the card,
2. registers the pre-seeded store paths in the nix database,
3. generates the SSH host keys,
4. joins the `Furgoneta de cacharrancia` Wi-Fi,

and from then on every boot is a normal boot of the installed system.
Later changes are regular `nixos-rebuild` generations on that same card.

## Bring-up

The Wi-Fi passphrase is kept in [`secrets.yaml`](secrets.yaml) (sops, key
`wifi_psk`) and is not part of the image: `flash-sd` decrypts it and
drops it on the card's firmware partition, where the host picks it up on
boot (see [`../common/wifi`](../common/wifi/default.nix)).

```sh
just flash-sd pitchen /dev/sdX   # builds the image, DESTROYS /dev/sdX
```

`flash-sd` is Linux-only; to flash from a Mac follow
["Flashing from a Mac"](../nidavellir/README.md#flashing-from-a-mac) in
the nidavellir README with `pitchen` in place of `nidavellir`.

If Wi-Fi does not come up, plug in a keyboard and check that
`/boot/firmware/wifi-secrets` exists: without it `wifi-secrets.service`
fails and `wpa_supplicant` never starts.

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
| wpa_supplicant | `wlan0` | passphrase copied from `/boot/firmware/wifi-secrets` on every boot |
| avahi | mDNS | publishes `pitchen.local` |
| tailscale | `tailscale0` | optional, `sudo tailscale up` once if wanted |

## Touch

Finger drag scrolls and pinch zooms, as on a tablet. This needs nothing
beyond what is here: cage hands touches to Chromium as touch events. The
Raspberry Pi OS install this replaces had labwc turn every touch into a
mouse pointer (`mouseEmulation="yes"`), which is why scrolling meant
hunting for scrollbars.

## Kernel

Board support comes from nixpkgs alone: its `sd-image-aarch64` module
ships the Pi 5 device trees and a U-Boot that knows the board, and the
mainline kernel drives the SD slot, HDMI, Wi-Fi and USB. Everything is in
the binary cache, so nothing is compiled.

The kernel is `linuxPackages_latest` rather than the default LTS: the
Pi 5's GPU only got its mainline device tree node after 6.18, and without
it cage and Chromium render in software. Go back to the default once the
LTS is new enough.

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

Only the Wi-Fi passphrase, encrypted to the user key and decrypted on the
workstation at flash time (`just edit-secrets pitchen` to change it; it
is the same network as `nidavellir`, so keep the two in step). The host
itself decrypts nothing, so sops-nix is not wired in. If it grows secrets
of its own, follow the same post-first-boot rekey as the other machines
(`just age-gen pitchen`, add `&host-pitchen` to `.sops.yaml`).
