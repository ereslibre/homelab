# pi-desktop (diskless iSCSI netboot)

`pi-desktop` is a Raspberry Pi 400 that carries no local storage. The Pi's
VideoCore firmware loads the kernel + initrd over **TFTP** from the Synology
(`10.0.4.2`), the initrd logs into an **iSCSI** target on the same Synology,
and pivots root onto the LUN.

## Synology (DS923+, DSM 7.3) — one-time setup

1. **SAN Manager → LUN**: create `pi-desktop-root`, 128 GB, thin-provisioned,
   on the desired volume.
2. **SAN Manager → Target**: create the target, bind the LUN above, and
   restrict the ACL to initiator IQN `iqn.2026-04.net.ereslibre:pi-desktop`.
   DSM auto-generates the target IQN; the current one is
   `iqn.2000-01.com.synology:synology.default-target.ca49c4149b2` (also
   referenced by `hardware-configuration.nix`).
3. **Package Center → TFTP Server**: install, enable, and point it at a shared
   folder such as `/volume1/pis`. With `TFTP_PREFIX=1` and
   `TFTP_PREFIX_STR=pi-desktop/` (set in step "Pi EEPROM" below — note
   the trailing slash) the firmware loads boot files from
   `<root>/pi-desktop/`, so pi-desktop's files live in
   `/volume1/pis/pi-desktop/`. Inside that subdirectory the
   Pi firmware expects the standard Raspberry Pi boot files
   (`bootcode.bin`, `start4.elf`, `fixup4.dat`, `config.txt`,
   `cmdline.txt`, `kernel8.img`, `initrd`, `bcm2711-rpi-4-b.dtb`, …). The
   `kernel8.img` / `initrd` / `cmdline.txt` are produced by `nixos-rebuild`;
   the rest come from the `raspberrypifw` package. See "Populating the TFTP
   tree" below.

## Pi EEPROM — one-time flash (currently HDD-only)

The stock Pi 400 EEPROM boots from SD/USB only. **The SD slot on this
particular Pi 400 is physically broken (auto-ejects), so always use a
USB drive** — boot the Pi once from a USB stick running Raspberry Pi OS
(or the NixOS aarch64 installer) and run:

```
# rpi-eeprom-config --edit
```

Set:

```
BOOT_ORDER=0xf42              # LSB-first: 2=NETWORK, 4=USB-MSD, f=RESTART (loop). Skip SD (broken).
TFTP_IP=10.0.4.2              # override DHCP server-ip; Synology serves TFTP here
TFTP_PREFIX=1                 # 1 = use TFTP_PREFIX_STR (literal); 0 = serial-number, 2 = MAC
TFTP_PREFIX_STR=pi-desktop/   # trailing slash REQUIRED — string is concatenated as-is to the filename
BOOT_UART=1                   # bootloader logs to GPIO 14/15 @ 115200 8N1 (always-on for diagnosability)
NET_INSTALL_AT_POWER_ON=0     # default firmware images ship with =1; force off for unattended netboot
DISABLE_HDMI=0                # leave HDMI diagnostics screen enabled
```

`BOOT_ORDER` is a 32-bit integer read nibble-by-nibble from the LSB. Per the
[official Pi bootloader docs](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#BOOT_ORDER):
`0x1`=SD, `0x2`=NETWORK, `0x3`=RPIBOOT, `0x4`=USB-MSD, `0x6`=NVMe (Pi 5/CM4),
`0x7`=HTTP, `0xe`=STOP, `0xf`=RESTART (loop back to the first nibble). So
`0xf42` tries NETWORK first, falls back to USB-MSD, then loops. **A USB
installer plugged in is reachable via the `4` nibble as fallback — do not
place `4` before `2` (e.g. `0xf24`), or the bootloader will boot USB first
and never attempt netboot.**

`TFTP_PREFIX` selects the per-device TFTP subdirectory (see the
[official docs](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#TFTP_PREFIX)):
`0`=serial number (default), `1`=string from `TFTP_PREFIX_STR`, `2`=MAC address.
`TFTP_PREFIX_STR` is consulted **only** when `TFTP_PREFIX=1` (max 32 chars)
and is **prepended literally** to the requested filename — there is no
implicit `/`. Without a trailing slash, the bootloader requests e.g.
`pi-desktopstart4.elf` and every TFTP fetch 404s.

**Firmware version warning.** Stock `raspberrypi-eeprom-2026.01.09-2711`
(currently default in nixpkgs) is broken on this Pi 400 — the bootloader
emits zero packets on the wire during its netboot window, despite a
correct `BOOT_ORDER=0xf42`. Flash with an older firmware image instead:
download `pieeprom-2025-05-08.bin` (or a verified newer good one) from
[rpi-eeprom/firmware-2711/latest](https://github.com/raspberrypi/rpi-eeprom/tree/master/firmware-2711/latest)
and pass it to `rpi-eeprom-config --apply` along with the config above.
Recipe (run on the Pi booted off the USB installer, as root):

```sh
mkdir -p /firmware && mount /dev/sda1 /firmware
curl -fL -o /tmp/pieeprom-old.bin \
  https://github.com/raspberrypi/rpi-eeprom/raw/master/firmware-2711/latest/pieeprom-2025-05-08.bin
curl -fL -o /tmp/recovery.bin \
  https://github.com/raspberrypi/rpi-eeprom/raw/master/firmware-2711/latest/recovery.bin
# write /tmp/pi-eeprom.conf with the BOOT_ORDER/TFTP_* block above, then:
nix shell nixpkgs#raspberrypi-eeprom --command sh -c '
  rpi-eeprom-config --config /tmp/pi-eeprom.conf --out /tmp/pieeprom-new.bin /tmp/pieeprom-old.bin
  rpi-eeprom-digest -i /tmp/pieeprom-new.bin -o /tmp/pieeprom.sig
'
cp /tmp/pieeprom-new.bin /firmware/pieeprom.upd
cp /tmp/pieeprom.sig     /firmware/pieeprom.sig
cp /tmp/recovery.bin     /firmware/recovery.bin
sync; umount /firmware; poweroff
```

Pull wall power, replug — the recovery shim flashes the SPI and reboots.
Verify with `vcgencmd bootloader_config` from
`nix shell nixpkgs#libraspberrypi`. After the flash, delete
`/firmware/pieeprom.upd`, `/firmware/pieeprom.sig`, `/firmware/recovery.bin`
or the next cold cycle will re-apply them.

From this point the Pi can run headless with no local storage.

## Populating the TFTP tree (first install)

Build the Pi's closure on `hulk` (aarch64 builder) and copy the boot payload
to the Synology TFTP share:

```
$ nix build 'github:ereslibre/homelab#nixosConfigurations.pi-desktop.config.system.build.toplevel'
$ TOPLEVEL=$(readlink -f ./result)
$ scp $TOPLEVEL/kernel          admin@10.0.4.2:/volume1/pis/pi-desktop/kernel8.img
$ scp $TOPLEVEL/initrd          admin@10.0.4.2:/volume1/pis/pi-desktop/initrd
$ cat $TOPLEVEL/kernel-params    # → goes into cmdline.txt on the TFTP share
```

`cmdline.txt` on the Synology must contain the kernel params above plus
`ip=dhcp root=/dev/disk/by-label/PIROOT rootfstype=ext4 rootwait`.

The static firmware blobs (`bootcode.bin`, `start4.elf`, `fixup4.dat`,
`bcm2711-rpi-4-b.dtb`, `config.txt`) are copied once from
`nix build nixpkgs#raspberrypifw` into the same share.

## First install into the LUN

The install needs an aarch64 Linux host that can `iscsiadm` into the
Synology. The cleanest path is to use the Pi itself, booted off an SD
card running the upstream NixOS aarch64 minimal installer. macOS and
Docker/Lima/linux-builder do **not** work for this — iSCSI needs a real
Linux kernel with `iscsi_tcp`, root, and mount privileges.

### A. Write the installer SD card (on a Mac, but any host with `dd` works)

Build the aarch64 NixOS minimal installer SD image from this flake's
pinned `nixpkgs` so the revision matches everything else (and the
substituter cache hits). On a Mac this offloads to `nix.linux-builder`
automatically.

```sh
cd <homelab-checkout>

nix build --impure --expr '
  let
    flake = builtins.getFlake (toString ./.);
    pkgs = flake.inputs.nixpkgs;
  in (pkgs.lib.nixosSystem {
    system = "aarch64-linux";
    modules = [
      "${pkgs}/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix"
    ];
  }).config.system.build.sdImage
' --print-out-paths

ls result/sd-image/
zstd -d result/sd-image/nixos-image-sd-card-*.img.zst -o /tmp/nixos-sd.img

# Find the SD card device — be careful, this overwrites it.
diskutil list                       # macOS
# lsblk                             # Linux

diskutil unmountDisk /dev/diskN
sudo dd if=/tmp/nixos-sd.img of=/dev/rdiskN bs=4m status=progress
sync && diskutil eject /dev/diskN
```

### B. Run the install from the Pi (booted off the SD card)

```sh
# Optional: bring up SSH so you can drive the rest from your laptop.
sudo -i
passwd                              # set a temp password
systemctl start sshd
ip a                                # note the Pi's IP

# Log into the Synology iSCSI target.
modprobe iscsi_tcp
systemctl start iscsid
iscsiadm -m discovery -t st -p 10.0.4.2
iscsiadm -m node \
  -T iqn.2000-01.com.synology:synology.default-target.ca49c4149b2 \
  -p 10.0.4.2 --login
lsblk                               # confirm /dev/sda appeared

# Format and mount.
mkfs.ext4 -L PIROOT /dev/sda
mkdir -p /mnt
mount /dev/disk/by-label/PIROOT /mnt

# Install pi-desktop's closure into the LUN.
nixos-install \
  --flake "github:ereslibre/homelab#pi-desktop" \
  --no-bootloader --no-root-passwd \
  --root /mnt

# Clean up.
umount /mnt
iscsiadm -m node \
  -T iqn.2000-01.com.synology:synology.default-target.ca49c4149b2 \
  -p 10.0.4.2 --logout
poweroff
```

`--no-bootloader` is mandatory: the Pi's bootloader is the TFTP tree,
not anything `nixos-install` would write.

After `poweroff`, pull the SD card. The next cold boot will go straight
to the EEPROM's network-boot path (configured in the next section).

## Re-key sops after first boot

The old `host-pi-desktop` age recipient in `.sops.yaml` belongs to a dead host
key. After the first successful boot:

```
$ ssh pi-desktop sudo ssh-to-age < /etc/ssh/ssh_host_ed25519_key.pub
# update .sops.yaml with the new recipient, then:
$ sops updatekeys pi-desktop/secrets.yaml
```

## Updating pi-desktop

Because the kernel + initrd live on TFTP but the rest of the system lives
on the iSCSI LUN, updates are two steps: switch the running system, then
refresh the TFTP boot files. **Do them in that order** — see the
ordering note at the bottom.

### 1. Switch the running system

On the Pi (over SSH from anywhere):

```sh
ssh pi-desktop sudo nixos-rebuild \
  --flake "github:ereslibre/homelab#pi-desktop" --no-write-lock-file switch
```

`--no-write-lock-file` is required: `nixos-rebuild` would otherwise try
to write a lock-file update back into the GitHub source and abort. The
flake's lock-file is regenerated on every `nix flake update` in this
repo, so the warning it emits about "missing inputs" is harmless.

This builds (or substitutes) the new closure into `/nix/store` on the
LUN, activates it as `/run/current-system`, and switches systemd over.
Userspace is now the new generation. The running kernel is still the
*old* one — kernel/initrd swaps require a hardware reboot, which step 2
prepares.

### 2. Refresh the TFTP boot files

Push the new kernel, initrd, and a fresh `cmdline.txt` to the Synology
TFTP share so the next cold boot picks up the new generation:

From a workstation that can ssh to both pi-desktop and the Synology:

```sh
# Build cmdline.txt locally, with the new toplevel's init path.
ssh pi-desktop '
  TOPLEVEL=$(readlink -f /run/current-system)
  echo "init=$TOPLEVEL/init $(cat $TOPLEVEL/kernel-params) ip=dhcp root=/dev/disk/by-label/PIROOT rootfstype=ext4 rootwait"
' > /tmp/cmdline.txt

# Stream kernel and initrd through the workstation (avoids needing
# pi-desktop -> Synology SSH).
ssh pi-desktop 'cat /run/current-system/kernel' > /tmp/kernel8.img
ssh pi-desktop 'cat /run/current-system/initrd' > /tmp/initrd

# Publish to the Synology TFTP share.
scp /tmp/kernel8.img /tmp/initrd /tmp/cmdline.txt admin@10.0.4.2:/volume1/pis/pi-desktop/
```

`cmdline.txt` must contain:

- `init=<new-toplevel>/init` — points the kernel at the new generation's
  init binary. **This is the critical part of the update** — it's what
  pivots boot from the old generation to the new one.
- the contents of `<new-toplevel>/kernel-params` — the
  `boot.kernelParams` from the NixOS config.
- `ip=dhcp root=/dev/disk/by-label/PIROOT rootfstype=ext4 rootwait` —
  the iSCSI root setup. These are static; they don't change between
  generations.

The Pi VideoCore firmware appends its own params (`coherent_pool=1M`,
`bcm2708_fb.*`, `smsc95xx.macaddr=…`, `vc_mem.*`) at boot — those come
from `start4.elf` and the DTB, not from `cmdline.txt`.

### 3. Verify

```sh
ssh pi-desktop sudo reboot
# wait ~30-60s, then:
ssh pi-desktop '
  echo "running toplevel: $(readlink /run/current-system)"
  echo "kernel: $(uname -r)"
  cat /proc/cmdline
'
```

The `init=` in `/proc/cmdline` should match the path of
`/run/current-system` — confirming the kernel booted into the new
generation, not the previous one.

### Why the ordering matters

Step 1 *first* puts the new toplevel into `/nix/store` on the LUN. Step
2 then writes a `cmdline.txt` whose `init=` points at that path. If you
swap the order and reboot in between, the new kernel boots, looks for
`init=<NEW_TOPLEVEL>/init`, doesn't find it (the LUN still has the old
generation), and kernel-panics. Done in the right order, an accidental
reboot between step 1 and step 2 is harmless — the old TFTP cmdline
points at the old (still-present) generation, so the system just comes
back up unchanged.
