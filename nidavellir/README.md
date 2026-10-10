# nidavellir (3D-printer host)

`nidavellir` is a Raspberry Pi Zero 2 W that drives the Ender 5 S1 over
USB with [OctoPrint](https://octoprint.org), with an optional USB webcam.
Unlike `pi-desktop` and the `cpi-N` fleet it has nothing to do with TFTP
or iSCSI: it boots from a plain SD card and its only network is Wi-Fi.

## How the SD card "installs" itself

There is no installer step. `just sd-image nidavellir` builds an image
that already *is* the installed system (firmware partition + ext4 root
carrying the full closure). On the very first boot NixOS:

1. grows the root partition and filesystem to fill the card,
2. registers the pre-seeded store paths in the nix database,
3. generates the SSH host keys,
4. joins the `Furgoneta de cacharrancia` Wi-Fi,

and from then on every boot is a normal boot of the installed system.
Later changes are regular `nixos-rebuild` generations on that same card.

The Zero 2 W has no boot EEPROM: it always boots from the SD slot, so
there is no `BOOT_ORDER` to worry about.

## Bring-up

The Wi-Fi passphrase is kept in `secrets.yaml` (sops, encrypted to the
user key). Set it once:

```sh
just edit-secrets nidavellir     # wifi_psk: <the passphrase>
```

Then, on hulk (binfmt aarch64 emulation), build the image and write the
card in one go:

```sh
just flash-sd nidavellir /dev/sdX
```

`flash-sd` refuses non-removable or mounted devices, `dd`s the image and
then decrypts the passphrase onto the card's FAT partition as
`wifi-secrets`. It is deliberately not baked into the image: that would
put it in the world-readable nix store.

Use a 16 GB or larger card. Insert it, cable the Pi up (below) and power
on. First boot takes several minutes on this board (partition resize).
It then takes a DHCP lease and publishes `nidavellir.local` over mDNS.
SSH is the only service open on the LAN:

```sh
ssh ereslibre@nidavellir.local
sudo tailscale up          # one-time, authenticate in the browser
```

After that OctoPrint is at `http://nidavellir/` from any tailnet device.
The first visit runs OctoPrint's setup wizard (create the admin user
there; printer profile for a stock Ender 5 S1 is 220 × 220 × 280 mm,
origin front-left, heated bed, 0.4 mm nozzle).

If it never shows up on the network, plug a mini-HDMI monitor in: the
console is on `tty0`, and `systemctl status wifi-secrets wpa_supplicant`
says whether the passphrase file was found. To change the passphrase
later, update `secrets.yaml` and also edit `wifi-secrets` on the card's
FAT partition from any computer; it is re-read on every boot.

## Cabling

The Zero 2 W has two micro-USB sockets and they are not interchangeable:

| Socket | Goes to |
|---|---|
| `PWR IN` (outer) | a 5 V / 2.5 A supply |
| `USB` (inner) | the printer's front USB-C port, via a micro-USB OTG cable or adapter |

That single data port is all there is, so a webcam needs a small OTG hub
shared with the printer.

## Case

[`case/case.scad`](case/case.scad) is a two-part printable case with ears
that bolt onto a 2020 slot of the printer frame. Render and print it:

```sh
cd nidavellir/case
nix shell nixpkgs#openscad-unstable -c openscad -D 'part="base"' -o base.stl case.scad
nix shell nixpkgs#openscad-unstable -c openscad -D 'part="lid"'  -o lid.stl  case.scad
```

Both parts print flat as exported, without supports (0.2 mm layers; PETG
if it sits near the bed or PSU, otherwise PLA is fine). Hardware:

- 4 × M2.5 × 12 screws — through the lid and the board, self-tapping
  into the base standoffs;
- 2 × M4 × 8 screws + M4 T-nuts for the frame (`mount_screw_d = 5.4`
  for M5, `mount = "none"` to drop the ears).

Cut-outs: mini-HDMI, both micro-USB sockets, the SD card (swappable
without opening the case), a slot over the 40-pin header and vents over
the SoC. The parameters at the top of the file cover fit (`clearance`),
the header slot and an optional camera-ribbon notch.

## What's on it

| Piece | Where | Notes |
|---|---|---|
| OctoPrint | `127.0.0.1:5000` | serial port auto-detected, 115200 baud, auto-connect |
| µStreamer | `127.0.0.1:8080` | only runs while a USB camera is plugged in |
| nginx | `:80` | `/` → OctoPrint, `/webcam/` → µStreamer |
| tailscale | `tailscale0` | the only interface allowed to reach `:80` |
| avahi | mDNS | publishes `nidavellir.local` |

To also expose the UI on the LAN, add
`networking.firewall.allowedTCPPorts = [80];` to `configuration.nix`.

OctoPrint state (users, uploaded G-code, timelapses, plugins installed
through the UI) lives in `/var/lib/octoprint` on the card. Settings
declared in `configuration.nix` are merged over `config.yaml` on every
service start, so they win over changes made in the UI.

## Webcam

Any UVC USB camera works, through an OTG hub (see Cabling). A udev rule
symlinks its capture node to `/dev/webcam` and starts `ustreamer.service`; unplugging stops it. The
stream is wired into OctoPrint's classic webcam plugin already. It is set
to 640 × 480 at 10 fps to leave the CPU to OctoPrint; tune it in
`services.ustreamer.extraArgs`.

## Updating

Same flow as a running `cpi-N` — build on hulk, push, activate:

```sh
just build nidavellir
TOPLEVEL=$(readlink -f ./result)
nix copy --to ssh-ng://ereslibre@nidavellir "$TOPLEVEL"
ssh -t ereslibre@nidavellir "sudo nix-env -p /nix/var/nix/profiles/system --set $TOPLEVEL && sudo $TOPLEVEL/bin/switch-to-configuration switch"
```

Never build *on* the Pi — 512 MB of RAM is not enough to evaluate the
flake, let alone compile. Don't do this mid-print if the change restarts
`octoprint.service`.

## Secrets

Only the Wi-Fi passphrase, in `secrets.yaml`. It is encrypted to the user
key alone and decrypted on the workstation at flash time (see Bring-up),
because the Pi needs it before it has a host key to decrypt with.
sops-nix is not wired in. If the host grows secrets, follow
the same post-first-boot rekey as the other machines (`just age-gen
nidavellir`, add `&host-nidavellir` to `.sops.yaml`).
