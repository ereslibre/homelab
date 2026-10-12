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
SSH and the web UI are open on the LAN:

```sh
ssh ereslibre@nidavellir.local
```

OctoPrint is at `http://nidavellir.local/`. The first visit runs
OctoPrint's setup wizard: create the admin user there and fill in the
printer profile from [Printer profile](#printer-profile).

The host is deliberately not on the tailnet: from outside, the network
is reached through nuc-1. `nidavellir.local` only resolves from machines
that speak mDNS (macOS does; hulk does not), so elsewhere use the
address from the router's client list.

To change the passphrase later, update `secrets.yaml` and also edit
`wifi-secrets` on the card's FAT partition from any computer; it is
re-read on every boot.

### Flashing from a Mac

hulk has no card reader, and `flash-sd` is Linux-only (`lsblk`,
`blockdev`, `udevadm`). Build on hulk with `just sd-image nidavellir`,
then do the rest by hand on the Mac, from a checkout of this repo:

```sh
scp 'hulk:<checkout>/result/sd-image/*.img.zst' ~/Downloads/

diskutil list external physical          # find the card: /dev/diskN
diskutil unmountDisk /dev/diskN
nix shell nixpkgs#zstd -c zstd -dc ~/Downloads/nixos-image-sd-card-*.img.zst \
  | sudo dd of=/dev/rdiskN bs=4m
sync
```

macOS then mounts the firmware partition as `/Volumes/FIRMWARE`
(reinsert the card if it does not). Seed the passphrase, and look at the
result before ejecting:

```sh
ls /Volumes/FIRMWARE/config.txt          # the right volume is mounted
PSK=$(nix develop --accept-flake-config --command sops decrypt --extract '["wifi_psk"]' nidavellir/secrets.yaml)
echo ${#PSK}                             # 0 means the decrypt failed
printf 'psk_furgoneta=%s\n' "$PSK" > /Volumes/FIRMWARE/wifi-secrets
cat /Volumes/FIRMWARE/wifi-secrets
diskutil eject /Volumes/FIRMWARE
```

The file must hold exactly one `psk_furgoneta=<passphrase>` line:
`psk_furgoneta` is the name `configuration.nix` looks the passphrase up
under (`pskRaw = "ext:psk_furgoneta"`), not the sops key. Writing the
image wipes the file, so repeat this after every reflash.

### When Wi-Fi does not come up

Without `wifi-secrets` on the card, `wifi-secrets.service` fails and
`wpa_supplicant` never starts — check that first. The radio is 2.4 GHz
only and cannot do WPA3, so the network has to offer 2.4 GHz and WPA2.

The board has no USB-A socket for a keyboard, so there are two ways to
see what is going on without one:

- **Serial console over USB.** Connect the inner `USB` socket to a
  computer with a plain micro-USB data cable (no OTG adapter); that
  cable also powers the Pi. It shows up as a serial port with a login
  prompt:

  ```sh
  ls /dev/cu.usbmodem*                   # Linux: /dev/ttyACM*
  screen /dev/cu.usbmodemXXXX 115200
  ```

  Log in and run `systemctl status wifi-secrets wpa_supplicant`. Things
  that trip this up:

  - It has to be the `USB` socket; `PWR IN` has no data lines.
  - It has to be a data cable. With a charge-only one the Pi powers up
    but no serial port appears.
  - A laptop port is enough to run the bare board. If it resets, also
    plug the normal supply into `PWR IN`.
  - Only the login prompt is on this console, not the boot messages, and
    only once the system is up: a minute or two, longer on first boot.
    Press Enter if the screen stays blank.

- **Boot report.** About 45 s, 2 min and 4 min after boot the Pi
  rewrites `boot-report.txt` on the firmware partition: interfaces, the
  secrets file's key name and length (never the passphrase), unit
  status, a scan of visible networks, and the kernel and
  `wpa_supplicant` logs. Wait four minutes, move the card to another
  computer and read `/Volumes/FIRMWARE/boot-report.txt`.

A mini-HDMI monitor shows the console on `tty0` too, which is enough to
tell "does not boot" from "boots but is not on the network".

## Cabling

The Zero 2 W has two micro-USB sockets and they are not interchangeable:

| Socket | Goes to |
|---|---|
| `PWR IN` (outer) | a 5 V / 2.5 A supply |
| `USB` (inner) | the printer's front USB-C port, via a micro-USB OTG cable or adapter |

That single data port is all there is, so a webcam needs a small OTG hub
shared with the printer. Without the OTG adapter the same socket is a
serial console (see "When Wi-Fi does not come up").

## Case

[`case/`](case/) holds a two-part printable case with ears that bolt
onto a 2020 slot of the printer frame. Render the STL files for the
slicer with:

```sh
cd nidavellir/case
just
```

[`case/README.md`](case/README.md) covers printing, the hardware needed
and the parameters of the model.

## What's on it

| Piece | Where | Notes |
|---|---|---|
| OctoPrint | `127.0.0.1:5000` | serial port auto-detected, 115200 baud, auto-connect |
| µStreamer | `127.0.0.1:8080` | only runs while a USB camera is plugged in |
| nginx | `:80`, open on the LAN | `/` → OctoPrint, `/webcam/` → µStreamer |
| avahi | mDNS | publishes `nidavellir.local` |
| serial getty | `ttyGS0` | USB gadget console on the data socket |
| boot-report | `/boot/firmware/boot-report.txt` | network bring-up report, rewritten each boot |

OctoPrint state (users, uploaded G-code, timelapses, plugins installed
through the UI) lives in `/var/lib/octoprint` on the card. Settings
declared in `configuration.nix` are merged over `config.yaml` on every
service start, so they win over changes made in the UI.

## Printer profile

The setup wizard asks for these; they are for a stock Ender 5 S1 (Sprite
direct-drive extruder, 0.4 mm nozzle). The profile is kept in
`/var/lib/octoprint`, not in `configuration.nix`.

| Tab | Setting | Value |
|---|---|---|
| General | Name | `Ender 5 S1` |
| General | Model | `Creality Ender 5 S1` |
| Print bed & build volume | Form factor | Rectangular |
| Print bed & build volume | Origin | Lower left |
| Print bed & build volume | Heated bed | yes |
| Print bed & build volume | Heated chamber | no |
| Print bed & build volume | Width (X) | 220 mm |
| Print bed & build volume | Depth (Y) | 220 mm |
| Print bed & build volume | Height (Z) | 280 mm |
| Print bed & build volume | Custom bounding box | no |
| Axes | X | 6000 mm/min, not inverted |
| Axes | Y | 6000 mm/min, not inverted |
| Axes | Z | 200 mm/min, not inverted |
| Axes | E | 300 mm/min, not inverted |
| Hotend & extruder | Nozzle diameter | 0.4 mm |
| Hotend & extruder | Number of extruders | 1 |
| Hotend & extruder | Default extrusion length | 5 mm |

The axes values are OctoPrint's defaults and only drive the jog buttons
in the Control tab, not prints. The Ender 5 homes X and Y to the back
right, but the coordinate origin is still front-left, so "Lower left" is
correct. The bed moves instead of the gantry, so jogging Z up lowers the
bed; tick "Invert control" on Z if that feels backwards.

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
PI=<address>     # hulk does not resolve nidavellir.local: use the lease's IP
nix copy --no-check-sigs --to ssh-ng://ereslibre@$PI "$TOPLEVEL"   # locally built, so unsigned
ssh -t ereslibre@$PI "sudo nix-env -p /nix/var/nix/profiles/system --set $TOPLEVEL && sudo $TOPLEVEL/bin/switch-to-configuration switch"
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
