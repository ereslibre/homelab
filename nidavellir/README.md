# nidavellir (3D-printer host)

`nidavellir` is a Raspberry Pi Zero 2 W that drives the Ender 3 over USB
with [OctoPrint](https://octoprint.org), with an optional USB webcam.
Unlike `pi-desktop` and the `cpi-N` fleet it has nothing to do with TFTP
or iSCSI: it boots from a plain SD card and lives on Wi-Fi.

## How the SD card "installs" itself

There is no installer step. `just sd-image nidavellir` builds an image
that already *is* the installed system (firmware partition + ext4 root
carrying the full closure). On the very first boot NixOS:

1. grows the root partition and filesystem to fill the card,
2. registers the pre-seeded store paths in the nix database,
3. generates the SSH host keys,
4. reads `wifi.env` from the firmware partition and joins the Wi-Fi,

and from then on every boot is a normal boot of the installed system.
Later changes are regular `nixos-rebuild` generations on that same card.

## Bring-up

```sh
# On hulk (binfmt aarch64 emulation)
just sd-image nidavellir
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

Insert the card, connect the printer to the Zero's data port (the inner
micro-USB one, through an OTG adapter) and power the Pi on its other
port. First boot takes a few minutes (partition resize, on a slow
board). SSH is the only thing open on the LAN:

```sh
ssh ereslibre@nidavellir.local
sudo tailscale up          # one-time, authenticate in the browser
```

After that OctoPrint is at `http://nidavellir/` from any tailnet device.
The first visit runs OctoPrint's setup wizard (create the admin user
there; printer profile for a stock Ender 3 is 220 × 220 × 250 mm, heated
bed, 0.4 mm nozzle).

The Zero 2 W has no boot EEPROM to configure: it only boots from the SD
card.

## Living in 512 MB

The board has 512 MB of RAM, which is enough to run OctoPrint but not to
build for it:

- Never `nixos-rebuild` on the device; evaluation alone runs it out of
  memory. Build elsewhere and copy the closure (see *Updating*).
- Swap is compressed RAM (zram) sized to the whole of RAM.
- Keep OctoPrint plugins light and render timelapses elsewhere.

## What's on it

| Piece | Where | Notes |
|---|---|---|
| OctoPrint | `127.0.0.1:5000` | serial port auto-detected, 115200 baud, auto-connect |
| µStreamer | `127.0.0.1:8080` | only runs while a USB camera is plugged in |
| nginx | `:80` | `/` → OctoPrint, `/webcam/` → µStreamer |
| tailscale | `tailscale0` | the only interface allowed to reach `:80` |
| NetworkManager | `wlan0` | one profile, `home`, filled in from `/boot/firmware/wifi.env` |
| avahi | mDNS | publishes `nidavellir.local` |

To also expose the UI on the LAN, add
`networking.firewall.allowedTCPPorts = [80];` to `configuration.nix`.

OctoPrint state (users, uploaded G-code, timelapses, plugins installed
through the UI) lives in `/var/lib/octoprint` on the card. Settings
declared in `configuration.nix` are merged over `config.yaml` on every
service start, so they win over changes made in the UI.

## Webcam

Any UVC USB camera works, but the Zero has a single USB data port and the
printer takes it, so a camera needs a small USB hub. A udev rule symlinks
its capture node to `/dev/webcam` and starts `ustreamer.service`;
unplugging stops it. The stream is wired into OctoPrint's classic webcam
plugin already. It is set to 640 × 480 at 10 fps to leave the CPU to
OctoPrint; tune it in `services.ustreamer.extraArgs`.

## The kernel is built locally

The Zero 2 W needs the Raspberry Pi vendor kernel and firmware, which
come from the [`nixos-raspberrypi`](https://github.com/nvmd/nixos-raspberrypi)
flake rather than `nixos-hardware`. The repo follows nixos-unstable, so
the input tracks that flake's `nixos-unstable` branch, and its binary
cache only carries kernels for the stable branches. Expect the first
`just sd-image nidavellir`, and any later one after bumping that input,
to compile the kernel: long under emulation on hulk, much quicker on a
native aarch64 builder.

## Updating

Same flow as a running `cpi-N` — build on hulk, push, activate:

```sh
just build nidavellir
TOPLEVEL=$(readlink -f ./result)
nix copy --to ssh-ng://ereslibre@nidavellir.local "$TOPLEVEL"
ssh -t ereslibre@nidavellir.local "sudo nix-env -p /nix/var/nix/profiles/system --set $TOPLEVEL && sudo $TOPLEVEL/bin/switch-to-configuration switch"
```

Don't do this mid-print if the change restarts `octoprint.service`.

## Secrets

None in the repo, so sops-nix is not wired in; the Wi-Fi credentials live
only on the card. If the host grows secrets, follow
the same post-first-boot rekey as the other machines (`just age-gen
nidavellir`, add `&host-nidavellir` to `.sops.yaml`).
