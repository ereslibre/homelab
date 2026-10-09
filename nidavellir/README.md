# nidavellir (3D-printer host)

`nidavellir` is a Raspberry Pi 4 B that drives the Ender 3 over USB with
[OctoPrint](https://octoprint.org), with an optional USB webcam. Unlike
`pi-desktop` and the `cpi-N` fleet it has nothing to do with TFTP or
iSCSI: it boots from a plain SD card.

## How the SD card "installs" itself

There is no installer step. `just sd-image nidavellir` builds an image
that already *is* the installed system (firmware partition + ext4 root
carrying the full closure). On the very first boot NixOS:

1. grows the root partition and filesystem to fill the card,
2. registers the pre-seeded store paths in the nix database,
3. generates the SSH host keys,

and from then on every boot is a normal boot of the installed system.
Later changes are regular `nixos-rebuild` generations on that same card.

## Bring-up

```sh
# On hulk (binfmt aarch64 emulation)
just sd-image nidavellir
zstd -d --stdout result/sd-image/*.img.zst | sudo dd of=/dev/sdX bs=4M status=progress conv=fsync
```

Insert the card, plug in Ethernet and the printer's USB cable, power on.
First boot takes a couple of minutes (partition resize). The Pi takes a
DHCP lease on `end0`; SSH is the only thing open on the LAN:

```sh
ssh ereslibre@<dhcp-ip>
sudo tailscale up          # one-time, authenticate in the browser
```

After that OctoPrint is at `http://nidavellir/` from any tailnet device.
The first visit runs OctoPrint's setup wizard (create the admin user
there; printer profile for a stock Ender 3 is 220 × 220 × 250 mm, heated
bed, 0.4 mm nozzle).

The EEPROM must have SD in its `BOOT_ORDER` — a factory Pi 4 does
(`0xf41`). A Pi recycled from the `cpi-N` fleet does **not** (`0xf42`
is net → USB only); reflash it with a `1` nibble first, or boot the same
image from a USB stick instead.

## What's on it

| Piece | Where | Notes |
|---|---|---|
| OctoPrint | `127.0.0.1:5000` | serial port auto-detected, 115200 baud, auto-connect |
| µStreamer | `127.0.0.1:8080` | only runs while a USB camera is plugged in |
| nginx | `:80` | `/` → OctoPrint, `/webcam/` → µStreamer |
| tailscale | `tailscale0` | the only interface allowed to reach `:80` |

To also expose the UI on the LAN, add
`networking.firewall.allowedTCPPorts = [80];` to `configuration.nix`.

OctoPrint state (users, uploaded G-code, timelapses, plugins installed
through the UI) lives in `/var/lib/octoprint` on the card. Settings
declared in `configuration.nix` are merged over `config.yaml` on every
service start, so they win over changes made in the UI.

## Webcam

Any UVC USB camera works. A udev rule symlinks its capture node to
`/dev/webcam` and starts `ustreamer.service`; unplugging stops it. The
stream is wired into OctoPrint's classic webcam plugin already. Tune
resolution / fps in `services.ustreamer.extraArgs`.

## Updating

Same flow as a running `cpi-N` — build on hulk, push, activate:

```sh
just build nidavellir
TOPLEVEL=$(readlink -f ./result)
nix copy --to ssh-ng://ereslibre@nidavellir "$TOPLEVEL"
ssh -t ereslibre@nidavellir "sudo nix-env -p /nix/var/nix/profiles/system --set $TOPLEVEL && sudo $TOPLEVEL/bin/switch-to-configuration switch"
```

Don't do this mid-print if the change restarts `octoprint.service`.

## Secrets

None yet, so sops-nix is not wired in. If the host grows secrets, follow
the same post-first-boot rekey as the other machines (`just age-gen
nidavellir`, add `&host-nidavellir` to `.sops.yaml`).
