# pi-desktop netboot — in-progress state (handoff for next session)

Working through `README.md` "pi-desktop (diskless iSCSI netboot)" with Claude.
Boot chain works for steps 1-5 (DSM setup, TFTP staging, install into iSCSI
LUN). **Stuck on step 6**: the Pi 4 EEPROM bootloader emits zero packets on
the wire during its boot window, so network boot never even attempts DHCP.
Every cold boot falls through `BOOT_ORDER=0xf24` to the USB-MSD slot and
comes back up in the live aarch64 NixOS installer at `10.0.4.40`.

> **2026-04-27 finding — likely root cause.** Per the
> [official Pi bootloader docs](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#BOOT_ORDER),
> `BOOT_ORDER` nibbles are: `0x2`=NETWORK, `0x4`=USB-MSD (we had these reversed
> in the README). `0xf24` is read LSB-first as **USB-MSD → NETWORK → RESTART**,
> so with the installer USB plugged in the bootloader correctly boots USB
> *first* and never even attempts network. The "bootloader silent on the wire"
> behaviour is then expected, not a firmware bug. Reflash with `BOOT_ORDER=0xf42`
> (NETWORK → USB-MSD → RESTART) before further UART / firmware-downgrade
> debugging. Also flip `TFTP_PREFIX` from `2` (MAC) to `1` (literal string) — `2`
> means MAC-address prefix per the same docs, so `TFTP_PREFIX_STR=pi-desktop`
> has been dead config the whole time.

Pick up at the **"What to try next"** section below.

## Where we left off (2026-04-26)

**Pi 4 EEPROM is correctly flashed** — verified by full cold power cycle
(unplug power, wait, replug) plus deletion of `/firmware/pieeprom.upd`,
`/firmware/pieeprom.sig`, `/firmware/recovery.bin`. After that, the live
SPI EEPROM still reports our config:

```
BOOT_UART=0
WAKE_ON_GPIO=1
ENABLE_SELF_UPDATE=1
BOOT_ORDER=0xf24
TFTP_IP=10.0.4.2
TFTP_PREFIX=2
TFTP_PREFIX_STR=pi-desktop
DISABLE_HDMI=0
NET_INSTALL_AT_POWER_ON=0
```

bootloader version `2026/01/09 16:12:13` (commit
`d76c460359ec31b2fadf3b48e44599673095326f`), packaged as
`raspberrypi-eeprom-2026.01.09-2711` in current nixpkgs.

**But** every cold boot reaches the installer (USB-MSD), not pi-desktop.

## How we know the bootloader is silent

`tcpdump -i enp36s0f1 'ether host e4:5f:01:01:3b:24 or (port 67 or port
68 or port 69)'` on hulk (10.0.4.20, same wired /24) captured TWO full
boot cycles. Both showed the same pattern:

- Pi reboots → ~70 s of complete silence from MAC `e4:5f:01:01:3b:24`.
- Then a single `BOOTP/DHCP REQUEST` from `0.0.0.0:68 → 255.255.255.255:67`
  — that's the kernel-side DHCP of the SD-installer userland, not the
  bootloader.

The Synology TFTP log (`/var/log/opentftp.log`) corroborates: zero
requests from `10.0.4.40` ever; only my one test query from hulk
(`Client 10.0.4.20:47629 ... start4.elf, 4422 Blocks Served`).

DHCP DISCOVER is a broadcast, so hulk on the same /24 would see it if
the bootloader sent it. The bootloader emits nothing.

## TFTP tree audit (verified clean — not the blocker)

Last verified 2026-04-26. The staged tree at
`/volume1/pis/pi-desktop/` on the Synology is consistent with what the
Pi 4 EEPROM bootloader expects (path matches `TFTP_PREFIX=2 +
TFTP_PREFIX_STR=pi-desktop`):

| Item | State |
|---|---|
| Path | `/volume1/pis/pi-desktop/` |
| Pi firmware | `start4.elf` (2.2 MB), `fixup4.dat` (5 KB), `bootcode.bin` (52 KB) |
| DTBs | `bcm2711-rpi-400.dtb` (correct for Pi 400) + `bcm2711-rpi-4-b.dtb` |
| Overlays | `overlays/` present |
| `kernel8.img` | aarch64 Linux Image, `MZ` EFI-stub header + `ARMd` magic at 0x38 ✓ |
| `initrd` | zstd-compressed (`28 b5 2f fd` magic). start4.elf passes verbatim; kernel decompresses. Fine. |
| `config.txt` | `arm_64bit=1`, `enable_uart=1`, `kernel=kernel8.img`, `initramfs initrd followkernel`. No `device_tree=` line — firmware auto-picks the 400 DTB. |
| `cmdline.txt` | `kunit.enable=0 loglevel=4 lsm=landlock,yama,bpf init=/nix/store/sc4kqkp975482nxm4j9m38i1abgfspnj-nixos-system-pi-desktop-26.05.20260422.0726a0e/init ip=dhcp root=/dev/disk/by-label/PIROOT rootfstype=ext4 rootwait` |

So the bootloader, *if* it ever reached TFTP, would find a complete
and correct payload. The blocker is exclusively the EEPROM bootloader
phase before any of these files are consulted.

The post-handoff pi-desktop boot chain would be:
1. start4.elf reads config.txt, cmdline.txt, kernel, initrd, DTB.
2. Kernel boots with `ip=dhcp` (initrd-level DHCP for `end0`).
3. NixOS scripted initrd runs the iSCSI initiator with our IQN
   (`iqn.2026-04.net.ereslibre:pi-desktop`), logs into target
   `iqn.2000-01.com.synology:synology.default-target.ca49c4149b2` at
   `10.0.4.2`.
4. `/dev/disk/by-label/PIROOT` (the LUN) becomes available.
5. Pivot to LUN; `init=/nix/store/.../init` runs; full system comes up.

## What we ruled out

- **Switch / cable.** User confirmed: unmanaged switch, regular DHCP at
  10.0.4.1, runtime ethtool shows 1000 Mb/s Full Duplex on `end0` with
  no errors. Direct router connection wasn't tested but the user pushed
  back on the theory and the data agrees.
- **TFTP_PREFIX semantics.** I had it documented backwards in the README
  initially (`TFTP_PREFIX=0` was meant to be "literal string", but it's
  actually "use serial-number prefix"). Re-flashed with `TFTP_PREFIX=2`
  + `TFTP_PREFIX_STR=pi-desktop`. **2026-04-27 correction:** per the
  [official docs](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#TFTP_PREFIX)
  `2` is the MAC-address prefix, *not* literal-string. The literal-string
  mode is `TFTP_PREFIX=1`. `TFTP_PREFIX_STR=pi-desktop` has been dead
  config since the reflash. Re-reflash with `TFTP_PREFIX=1` next time the
  EEPROM is touched.
- **DSM TFTP server config.** Verified working from hulk.
- **DSM iSCSI ACL.** LUN can be logged in to from the Pi's IQN.
- **Synology DHCP.** Not in use; not required (Pi 4 EEPROM uses baked-in
  `TFTP_IP`, not DHCP option 66/next-server).
- **The flash didn't take.** Disproved by cold-power-cycle + vcgencmd
  bootloader_config returning the new values from the actual SPI chip.
- **Stale `recovery.bin` looping.** `/firmware/recovery.bin`,
  `pieeprom.upd`, `pieeprom.sig` deleted before the last cold cycle;
  config still persists, behaviour unchanged.

## Topology / current state

- **hulk** (10.0.4.20) — driver. No leftover background processes
  (verified). The pcap files `/tmp/pi-boot.pcap` and `/tmp/pi-boot2.pcap`
  are still on hulk as evidence — root-owned, root from `sudo tcpdump`.
- **pi-desktop** (10.0.4.40) — Pi 400 booted off external **USB SSD**
  running NixOS aarch64 minimal installer. Pi's `end0` MAC is
  `e4:5f:01:01:3b:24`. Root password is `0xdeadbeef` (set fresh by user
  this session; the live installer has no persistence so the password is
  reset on each fresh boot, but the user re-sets it). Has `nix-shell -p
  openiscsi` ready to use; iSCSI session was cleanly logged out at end of
  install.
- **Synology DS923+** (10.0.4.2) — LUN populated with the pi-desktop
  closure. TFTP tree at `/volume1/pis/pi-desktop/` complete (`bootcode.bin`,
  `start4.elf`, `fixup4.dat`, `bcm2711-rpi-400.dtb`, `bcm2711-rpi-4-b.dtb`,
  `overlays/`, `kernel8.img`, `initrd`, `cmdline.txt`, `config.txt`). LUN
  IQN: `iqn.2000-01.com.synology:synology.default-target.ca49c4149b2`.
  Runs `opentftp 1.66` (very old; serves files but doesn't log
  file-not-found).

## Code state

Branch `pi-desktop-netboot-fix` (commit `b183f6b7`) is pushed to
`origin/pi-desktop-netboot-fix`. Contains the netboot config fixes
(target IQN, `boot.initrd.systemd.enable=false`, drop
`networking.useDHCP=true`, README updates). The branch was further
edited locally on the user's Mac to gate `nix-ai-tools` behind a
per-host `aiTools` flag (`dotfiles/default.nix`, `dotfiles/home.nix`,
`dotfiles/packages.nix`, `flake.nix`); those changes are **NOT pushed**
yet.

The README's "Pi EEPROM" section was updated mid-session and now
specifies `TFTP_PREFIX=1` and `BOOT_ORDER=0xf42` (per the official Pi
bootloader docs — `1` is the literal-string prefix and `0xf42` reads
LSB-first as NETWORK → USB-MSD → RESTART). The currently-flashed EEPROM
on the Pi still has the older incorrect `TFTP_PREFIX=2` and
`BOOT_ORDER=0xf24` and needs to be reflashed.

The pi-desktop closure built on the Mac is at
`/nix/store/sc4kqkp975482nxm4j9m38i1abgfspnj-nixos-system-pi-desktop-26.05.20260422.0726a0e`
(both on the Mac and on the Pi installer's local store after `nix copy
--to ssh-ng://root@10.0.4.40`).

## What to try next

In order of cost-benefit:

### 1. Try a different bootloader firmware version

The 2026/01/09 firmware is fresh; could have a regression on this Pi
400's PHY init. Download an older `pieeprom-XXXX-XX-XX.bin` from
[rpi-eeprom releases](https://github.com/raspberrypi/rpi-eeprom/tree/master/firmware-2711/stable),
e.g. one from mid-2025, and re-stage:

```sh
# On the Pi installer, root:
mkdir -p /firmware && mount /dev/sda1 /firmware

nix-shell -p curl --run '
  curl -L -o /tmp/pieeprom-old.bin \
    https://github.com/raspberrypi/rpi-eeprom/raw/master/firmware-2711/stable/pieeprom-2025-04-08.bin
'

cat > /tmp/pi-eeprom.conf <<EOF
[all]
BOOT_UART=1
WAKE_ON_GPIO=1
ENABLE_SELF_UPDATE=1
BOOT_ORDER=0xf42              # NETWORK → USB-MSD → RESTART (per official docs)
TFTP_IP=10.0.4.2
TFTP_PREFIX=1                 # 1 = use TFTP_PREFIX_STR (literal); 2 would be MAC-address
TFTP_PREFIX_STR=pi-desktop
DISABLE_HDMI=0
NET_INSTALL_AT_POWER_ON=0
EOF

NIX_CONFIG="experimental-features = nix-command flakes" \
  nix shell nixpkgs#raspberrypi-eeprom --command \
  rpi-eeprom-config --apply /tmp/pi-eeprom.conf /tmp/pieeprom-old.bin

sync; umount /firmware; reboot   # cold-cycle better than soft reboot
```

Note the `BOOT_UART=1` change above — flip this any time you flash, in
case a UART adapter shows up later.

### 2. UART-level debugging (most informative)

USB-to-TTL adapter on Pi GPIO pins 8 (TX, GPIO 14), 10 (RX, GPIO 15),
6 (GND). 3.3 V logic — do not use a 5 V adapter. With `BOOT_UART=1` set
in EEPROM, the bootloader writes verbose log to UART at 115200 8N1,
including DHCP attempts, link state, error codes. That tells us
*exactly* why the bootloader is silent — whether the PHY isn't coming
up, whether DHCP DISCOVER is being sent and dropped at the bootloader's
own stack, etc.

### 3. NETCONSOLE (longer shot)

Pi 4 EEPROM supports `NETCONSOLE=remote_ip:port` to send debug logs over
UDP. Catch-22: requires the bootloader's network to be at least
partially working, which is what we're trying to debug. Worth a quick
try if (1) and (2) are unavailable. Set `NETCONSOLE=10.0.4.20:6666` and
run `nc -ul 6666` on hulk. If anything arrives, we have signal.

### 4. Use u-boot on the USB drive as the netboot stage

Sidestep the silent EEPROM bootloader entirely. The boot chain is
already two-stage — EEPROM falls through to USB-MSD, `start4.elf`
loads `/dev/sda1:/u-boot-rpi4.bin`, u-boot then reads
`/boot/extlinux/extlinux.conf` from `/dev/sda2` and chains to the SD
installer's kernel. The user observed u-boot starts and reports its
network as up, so u-boot's own netboot capabilities work fine.

Reconfigure u-boot to **TFTP-boot pi-desktop first**, falling back to
the local extlinux entry only if TFTP fails. Sketch:

- Replace (or augment) `/boot/extlinux/extlinux.conf` so the default
  label invokes a `boot.scr` that runs `dhcp; tftpboot $kernel_addr_r
  10.0.4.2:pi-desktop/kernel8.img; tftpboot $ramdisk_addr_r
  10.0.4.2:pi-desktop/initrd; tftpboot $fdt_addr_r
  10.0.4.2:pi-desktop/bcm2711-rpi-400.dtb; setenv bootargs <our
  cmdline>; booti $kernel_addr_r $ramdisk_addr_r:$filesize $fdt_addr_r`.
- Or use u-boot's `pxe` command (it understands extlinux-on-TFTP).
- Or generate a `boot.scr` via `mkimage` and place it at the path u-boot
  loads automatically.

End state: Pi boots from USB to u-boot, u-boot TFTPs the pi-desktop
kernel + initrd, kernel runs iSCSI initrd, root pivots to the LUN. USB
stays plugged in but is only ~30 MB of u-boot stub.

This bypasses the EEPROM-bootloader-silent issue entirely. Less elegant
than pure EEPROM TFTP boot, but functional and requires no new hardware
(no UART). Pursue this if the firmware-downgrade route in (1) also
fails.

### 5. Give up on pure netboot; keep USB SSD

Final fallback. Make the USB SSD a small dedicated boot drive with the
real pi-desktop kernel/initrd and skip TFTP entirely; iSCSI mount still
provides `/`. Same end state as (4), just without TFTP. Boring but
deterministic.

## Reminders / gotchas seen this session

- Pi 400's built-in SD card slot is **physically broken** — auto-ejects
  cards. Use USB media for any imaging on this Pi.
- The Mac's TLS to Cloudflare R2 (private nix substituter) gets
  intercepted by the Spanish ISP during LaLiga matches — watch for cert
  `core1.netops.test` errors. NordVPN works around it.
- hulk *is* the local host in this repo — don't `ssh hulk` from the
  homelab repo. Build aarch64 closures locally via `binfmt-aarch64` (slow
  via QEMU emulation — better to build on the Mac via
  `nix.linux-builder`).
- `vcgencmd bootloader_config` reads from the GPU's RAM cache. To verify
  a flash actually persisted, do a *full cold power cycle* (pull the
  wall power), not `reboot`.
- After EEPROM flash succeeds, manually delete `/firmware/pieeprom.upd`,
  `/firmware/pieeprom.sig`, `/firmware/recovery.bin` from the FAT
  partition. They sometimes don't auto-clean. (Already done in this
  session.)
- nixpkgs ship `raspberrypi-eeprom` as
  `nixpkgs#raspberrypi-eeprom` (not `rpi-eeprom`).
- nixpkgs aarch64 SD installer image is named
  `nixos-image-sd-card-26.05.…img.zst`, NOT `nixos-sd-image-…` (renamed
  in 26.05).
- `nixos-install` without a strict `set -e` guard will silently install
  to `/mnt`-as-a-dir on the live rootfs if the LUN mount fails. Always
  guard with `[ "$(findmnt -no SOURCE /mnt)" = "/dev/sdb" ] || exit 1`.
- `boot.iscsi-initiator` requires `boot.initrd.systemd.enable = false`
  in modern nixpkgs (default flipped to true). Already pinned in
  `pi-desktop/hardware-configuration.nix`.

## Task tracker (last snapshot)

| # | Status        | Subject |
|---|---------------|---------|
| 1 | completed     | DSM: create LUN, target & ACL |
| 2 | completed     | DSM: enable TFTP server |
| 3 | completed     | Build pi-desktop closure on hulk *(actually done on Mac)* |
| 4 | completed     | Stage TFTP payload to Synology |
| 5 | completed     | Install NixOS into the iSCSI LUN |
| 6 | in_progress   | Flash Pi EEPROM for net boot *(EEPROM flashed but Pi bootloader silent on the wire — see "What to try next")* |
| 7 | pending       | Boot test + re-key sops |

## Side issues to clean up later

- Two evaluation warnings in the closure (NixOS 26.05+):
  - `services.xserver.desktopManager.gnome.enable` →
    `services.desktopManager.gnome.enable`
  - `services.xserver.displayManager.gdm.enable` →
    `services.displayManager.gdm.enable`
- "Scripted initrd is deprecated and scheduled for removal in 26.11" —
  but `boot.iscsi-initiator` still requires it. Track upstream nixpkgs
  for systemd-stage-1 iSCSI support; switch back to
  `boot.initrd.systemd.enable = true` once available, otherwise
  pi-desktop will need a custom initrd by 26.11.
- Static DHCP reservation for the Pi's `end0` MAC
  (`e4:5f:01:01:3b:24`) at 10.0.4.1.
- Old `host-pi-desktop` age recipient in `.sops.yaml` is stale; re-key
  per README "Re-key sops after first boot" — but only after the Pi
  successfully netboots and we have a real new host key.
- Mac-side `aiTools` flag changes are uncommitted on the Mac; should be
  folded into `pi-desktop-netboot-fix` (or merged separately) so other
  hosts don't pull codex unnecessarily.
- Two old pcap files (`/tmp/pi-boot.pcap`, `/tmp/pi-boot2.pcap`) on
  hulk, root-owned. Sudo to delete when done.
- The README's `BOOT_ORDER` semantics note now matches the official
  Pi bootloader docs (`BOOT_ORDER=0xf42` = NETWORK → USB-MSD → RESTART;
  `0x2`=NETWORK, `0x4`=USB-MSD, `0xf`=RESTART/loop, `0xe`=STOP).
- Synology TFTP server is **opentftp 1.66 from 2003**. Old, but works.
  No need to replace unless we see correctness issues.
