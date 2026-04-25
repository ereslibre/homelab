# Personal homelab

## Bootstrap a machine

```
# sudo nixos-install --flake "github:ereslibre/homelab#<hostname>"
```

## Update a machine

```
# sudo nixos-rebuild --flake "github:ereslibre/homelab#$(hostname)" switch
```

## Specific node tailscale configuration

### nuc-1

```
# sudo tailscale up --accept-dns=false --accept-routes --advertise-routes=10.0.1.0/24,10.0.2.0/24,10.0.3.0/24,10.0.4.0/24
```

### nuc-2

```
# sudo tailscale up --accept-dns=false
```

### nuc-3

```
# sudo tailscale up --accept-dns=false
```

## pi-desktop (diskless iSCSI netboot)

`pi-desktop` is a Raspberry Pi 400 that carries no local storage. The Pi's
VideoCore firmware loads the kernel + initrd over **TFTP** from the Synology
(`10.0.4.2`), the initrd logs into an **iSCSI** target on the same Synology,
and pivots root onto the LUN.

### Synology (DS923+, DSM 7.3) — one-time setup

1. **SAN Manager → LUN**: create `pi-desktop-root`, 128 GB, thin-provisioned,
   on the desired volume.
2. **SAN Manager → Target**: create the target, bind the LUN above, and
   restrict the ACL to initiator IQN `iqn.2026-04.net.ereslibre:pi-desktop`.
   DSM auto-generates the target IQN; the current one is
   `iqn.2000-01.com.synology:synology.default-target.ca49c4149b2` (also
   referenced by `pi-desktop/hardware-configuration.nix`).
3. **Package Center → TFTP Server**: install, enable, and point it at a shared
   folder such as `/volume1/pis`. With `TFTP_PREFIX=0` and
   `TFTP_PREFIX_STR=pi-desktop` (set in step "Pi EEPROM" below) the
   firmware loads boot files from `<root>/pi-desktop/`, so pi-desktop's
   files live in `/volume1/pis/pi-desktop/`. Inside that subdirectory the
   Pi firmware expects the standard Raspberry Pi boot files
   (`bootcode.bin`, `start4.elf`, `fixup4.dat`, `config.txt`,
   `cmdline.txt`, `kernel8.img`, `initrd`, `bcm2711-rpi-4-b.dtb`, …). The
   `kernel8.img` / `initrd` / `cmdline.txt` are produced by `nixos-rebuild`;
   the rest come from the `raspberrypifw` package. See "Populating the TFTP
   tree" below.

### Pi EEPROM — one-time flash (currently HDD-only)

The stock Pi 400 EEPROM boots from SD/USB only. **The SD slot on this
particular Pi 400 is physically broken (auto-ejects), so always use a
USB drive** — boot the Pi once from a USB stick running Raspberry Pi OS
(or the NixOS aarch64 installer) and run:

```
# rpi-eeprom-config --edit
```

Set:

```
BOOT_ORDER=0xf24              # LSB-first: 4=NETWORK, 2=USB, f=stop. Skip SD (broken).
TFTP_IP=10.0.4.2
TFTP_PREFIX=0                 # use TFTP_PREFIX_STR as the literal subdir
TFTP_PREFIX_STR=pi-desktop    # → /pi-desktop/ on the TFTP server
DISABLE_HDMI=0
```

`BOOT_ORDER` is read nibble-by-nibble from the LSB: `1`=SD, `2`=USB-MSD,
`4`=NETWORK, `e`=loop, `f`=stop. So `0xf24` tries network first, falls
back to USB, then halts.

Save and reboot — the EEPROM re-flashes on the next boot. From this point the
Pi can run headless with no local storage.

### Populating the TFTP tree (first install)

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

### First install into the LUN

The install needs an aarch64 Linux host that can `iscsiadm` into the
Synology. The cleanest path is to use the Pi itself, booted off an SD
card running the upstream NixOS aarch64 minimal installer. macOS and
Docker/Lima/linux-builder do **not** work for this — iSCSI needs a real
Linux kernel with `iscsi_tcp`, root, and mount privileges.

#### A. Write the installer SD card (on a Mac, but any host with `dd` works)

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

#### B. Run the install from the Pi (booted off the SD card)

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

### Re-key sops after first boot

The old `host-pi-desktop` age recipient in `.sops.yaml` belongs to a dead host
key. After the first successful boot:

```
$ ssh pi-desktop sudo ssh-to-age < /etc/ssh/ssh_host_ed25519_key.pub
# update .sops.yaml with the new recipient, then:
$ sops updatekeys pi-desktop/secrets.yaml
```

### Updating pi-desktop

```
# sudo nixos-rebuild --flake "github:ereslibre/homelab#pi-desktop" switch
```

After the rebuild, re-publish `kernel8.img` / `initrd` / `cmdline.txt` to the
TFTP share so the next cold boot picks them up (the running system has
already switched to the new generation — the TFTP copy is only consulted at
hardware boot time).
