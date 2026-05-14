# Headless Pi fleet (`cpi-1` … `cpi-7`)

Bring-up procedure for the rack-mounted, no-HDMI, PoE-powered Pi 4 B
fleet. Same netboot + iSCSI architecture as pi-desktop — see
[`../../pi-desktop/README.md`](../../pi-desktop/README.md) for the
architectural background. This doc covers the **per-host steps** to
add a new headless Pi.

## What every cpi-N inherits

`cpi-N/configuration.nix` imports `common/headless-pi`, which gives:

- No local bootloader (TFTP supplies kernel + initrd)
- NetworkManager / systemd-networkd kept off `end0` (the initrd-stage
  `ip=dhcp` config persists through pivot; any userspace DHCP client
  taking over `end0` cold-boot kills the iSCSI session — lessons from
  pi-desktop encoded here)
- Static DNS to the pi-holes (`10.0.4.30`, `10.0.4.31`) and the lab
  search domains
- Timezone / locale / console keymap
- No display chain, no GDM, no Wayland, no vc4 — purely headless

Each `cpi-N/` directory adds:

- `configuration.nix` — hostname + sops file (post-rekey) + imports
- `hardware-configuration.nix` — iSCSI initiator IQN + target IQN
- `secrets.yaml` (added after first boot, post sops rekey)

## TFTP namespacing

The fleet uses **MAC-based `TFTP_PREFIX=2`**: one identical EEPROM
config across all 7 Pis. Each Pi automatically looks for its boot
files at `/volume1/pis/<mac-with-dashes>/` on the Synology.

| Host | IP (router DHCP reservation) | MAC | TFTP dir |
|---|---|---|---|
| cpi-1 | 10.0.4.50 | `dc:a6:32:b1:07:03` | `/volume1/pis/dc-a6-32-b1-07-03/` |
| cpi-2 | 10.0.4.51 | `dc:a6:32:b1:06:f7` | `/volume1/pis/dc-a6-32-b1-06-f7/` |
| cpi-3 | 10.0.4.52 | `e4:5f:01:1b:25:31` | `/volume1/pis/e4-5f-01-1b-25-31/` |
| cpi-4 | 10.0.4.53 | `dc:a6:32:e2:1f:14` | `/volume1/pis/dc-a6-32-e2-1f-14/` |
| cpi-5 | 10.0.4.54 | `e4:5f:01:1b:25:eb` | `/volume1/pis/e4-5f-01-1b-25-eb/` |
| cpi-6 | 10.0.4.55 | `dc:a6:32:b1:06:e3` | `/volume1/pis/dc-a6-32-b1-06-e3/` |
| cpi-7 | 10.0.4.56 | `dc:a6:32:b1:06:df` | `/volume1/pis/dc-a6-32-b1-06-df/` |

`scripts/deploy-tftp.sh` already knows this mapping (`MAC_OF`); update
that array whenever the fleet changes.

## One-time prerequisites

These were done during cpi-1 bring-up and are reusable across the whole
fleet — skip if already in place.

1. **Universal aarch64 installer USB**: `just installer aarch64` →
   `dd` `result/sd-image/*.img.zst` (after `zstd -d`) to a USB-SSD.
   The image already has the `ereslibre@kde.org` SSH key baked in for
   root, so the installer is reachable over `ssh root@10.0.4.X`
   immediately on boot — no console required.
2. **EEPROM-recovery SD card** (one per Pi, *once*): Raspberry Pi
   Imager → *Misc utility images* → *Pi 4 EEPROM Boot Recovery* →
   USB-boot preset (any preset that includes USB in the order works).
   Boot each Pi off this SD once to flip its factory-default
   net-only `BOOT_ORDER` so USB fallback exists. After this, the SD
   is irrelevant.
3. **Static DHCP reservation**: already configured in
   `~/.network-config/routers/homerouter/config.boot` for all seven
   MACs.

## Bring-up procedure for a new cpi-N

Assumes prerequisites above are done and the previously-bootstrapped
cpi-1 is the canonical template.

### 1. DSM SAN Manager — LUN + target + ACL

- **Create new target named exactly `cpi-N`** (not `Target-K` —
  that's DSM's default but produces opaque IQNs). The IQN DSM
  generates will then look like
  `iqn.2000-01.com.synology:synology.cpi-N.<random-suffix>` — copy
  that string, it goes into `hardware-configuration.nix`.
- **Create new LUN** `cpi-N-root`, 128 GB (or whatever), thin-
  provisioned. **Tick "Enable Space Reclamation".** This setting
  cannot be changed post-creation — if you forget, the LUN will fill
  irrecoverably to its declared size on first `mkfs.ext4` (see TODO
  below for cpi-1's stuck LUN).
- **Bind LUN to the new target.**
- **Set the target's ACL** to allow only initiator IQN
  `iqn.2026-04.net.ereslibre:cpi-N`. Reject everything else.

### 2. Repo scaffolding

Each cpi-N is fully described by its integer index — hostname,
initiator IQN, target IQN, and all wiring come from
[`common/headless-pi/cpi-node.nix`](./cpi-node.nix) via the `mkCpi`
helper in `flake.nix`. No per-host directory is needed unless/until
the host grows its own sops secrets.

To add a host:

- **`flake.nix`**: append the integer to the `cpiNodes` list inside
  the `mkCpi` block (e.g. `cpiNodes = [1 2 3 4 5 6 7 8];` for cpi-8).
- **`dotfiles/default.nix`**: append to the `cpiNodes` list inside
  the `mkCpiHM` block similarly.
- **`scripts/deploy-tftp.sh`**: add an entry to `MAC_OF` (cpi-1
  through cpi-7 are already there).
- **Router DHCP reservation**: add the static lease for the new MAC.
- Commit + push to `origin/main`. The installer pulls from there.

That's it for the repo side. For an existing cpi-N (1-7), all of the
above is already in place — proceed directly to step 3.

### 3. Boot the Pi off the installer USB

- Plug the installer USB-SSD into cpi-N in its rack slot.
- Power on. **The fleet is PoE-powered**: power arrives over the
  ethernet cable, so the link stays up for the entire boot — the
  "pull ethernet to force USB fallback" trick that works on
  separately-powered Pis is *not* available here.
- Wait ~60-90 s. Two cases:
  - **Fresh / universal EEPROM, no TFTP files yet staged at
    `/volume1/pis/<mac-with-dashes>/`** (normal cpi-N first boot):
    netboot fails fast and the bootloader falls through to USB. This
    is the expected path.
  - **EEPROM points at another host's TFTP tree** (e.g. a cross-
    contaminated USB previously flashed pi-desktop's config onto this
    Pi — see USB-hygiene gotcha below). Netboot will *succeed* into
    that host's rootfs over iSCSI instead of falling through. To
    force fall-through without unplugging ethernet, temporarily
    rename the offending TFTP dir on the Synology *before* powering
    the Pi on:

    ```sh
    ssh -t ereslibre@10.0.4.2 \
      'sudo mv /volume1/pis/<other-host-dir> /volume1/pis/<other-host-dir>.HOLD'
    ```

    Rename back once cpi-N's EEPROM has been reflashed in step 7.
- From hulk: `ssh root@10.0.4.<reserved-IP>` should accept your key.

If it doesn't come up, the EEPROM-recovery prereq (step 2 above) wasn't
applied to that Pi — boot it once off the recovery SD first.

### 4. Pre-build cpi-N's closure on hulk, push to the Pi

The Pi's network is shared with iSCSI; making it build emacs locally
saturates everything. Build on hulk's binfmt-aarch64 instead, then
push:

```sh
# On hulk
just build cpi-N                                                          # binfmt-aarch64 emulation; closure ends up in /nix/store on hulk
nix copy --no-check-sigs --to ssh-ng://root@10.0.4.<DHCP-IP> ./result    # full closure → Pi /nix/store
```

This copies the closure your hulk just built (which substituted from
`cache.numtide.com` / `nix-community.cachix.org` etc. for everything
buildable, only emulated-built what's missing) onto the installer
Pi's local nix store. `nixos-install` then finds everything already
present and only has to copy from the Pi's `/nix/store` to `/mnt/nix/store`.

### 5. iSCSI login + install

On the cpi-N installer (one big block over SSH):

```sh
ssh root@10.0.4.<DHCP-IP> '
set -e
mkdir -p /etc/iscsi
echo "InitiatorName=iqn.2026-04.net.ereslibre:cpi-N" > /etc/iscsi/initiatorname.iscsi

modprobe iscsi_tcp

NIX_CONFIG="experimental-features = nix-command flakes" \
  nix shell nixpkgs#openiscsi --command bash -c "
  iscsid &
  sleep 2
  iscsiadm -m discovery -t st -p 10.0.4.2
  iscsiadm -m node -T <CPI-N-TARGET-IQN> -p 10.0.4.2 --login
"
sleep 2
lsblk

# Format with -E nodiscard to keep the thin LUN actually thin.
mkfs.ext4 -E nodiscard -L PIROOT /dev/sdb
mkdir -p /mnt
mount /dev/disk/by-label/PIROOT /mnt
[ "$(findmnt -no SOURCE /mnt)" = "/dev/sdb" ] || { echo WRONG; exit 1; }

NIX_CONFIG="experimental-features = nix-command flakes" \
  nixos-install \
    --flake "github:ereslibre/homelab#cpi-N" \
    --no-bootloader --no-root-passwd \
    --root /mnt
'
```

If you see `error (ignored): SQLite database … is busy` — harmless,
ignore. If you see two `nix build` processes competing for `/mnt`
store locks (a zombie from a killed earlier install), `sudo kill -9`
the orphan and the live one resumes.

### 6. Stage cpi-N's TFTP files on the Synology

Clone pi-desktop's firmware blobs + DTB into cpi-N's MAC dir, then
overwrite kernel/initrd/cmdline via the deploy script:

```sh
# Synology side: clone pi-desktop firmware bits, drop the vc4 overlay.
ssh -t ereslibre@10.0.4.2 '
  set -e
  DST=/volume1/pis/<MAC-WITH-DASHES>
  sudo mkdir -p $DST
  sudo cp -a /volume1/pis/pi-desktop/. $DST/
  sudo tee $DST/config.txt > /dev/null <<EOF
arm_64bit=1
enable_uart=1
kernel=kernel8.img
initramfs initrd followkernel
EOF
'

# Hulk side: push the cpi-N closure's kernel + initrd + cmdline.
just deploy-tftp cpi-N
```

### 7. EEPROM flash on cpi-N for the universal config

On cpi-N (still booted off the installer USB):

```sh
ssh root@10.0.4.<DHCP-IP> '
set -e
mkdir -p /firmware
mountpoint -q /firmware || mount /dev/sda1 /firmware

cat > /tmp/pi-eeprom.conf <<EOF
[all]
BOOT_UART=1
WAKE_ON_GPIO=1
ENABLE_SELF_UPDATE=1
BOOT_ORDER=0xf42
TFTP_IP=10.0.4.2
TFTP_PREFIX=2
DISABLE_HDMI=0
NET_INSTALL_AT_POWER_ON=0
EOF

NIX_CONFIG="experimental-features = nix-command flakes" \
  nix shell nixpkgs#raspberrypi-eeprom --command sh -c "
    curl -fL -o /tmp/pieeprom-old.bin https://github.com/raspberrypi/rpi-eeprom/raw/master/firmware-2711/latest/pieeprom-2025-05-08.bin
    curl -fL -o /tmp/recovery.bin    https://github.com/raspberrypi/rpi-eeprom/raw/master/firmware-2711/latest/recovery.bin
    rpi-eeprom-config --config /tmp/pi-eeprom.conf --out /tmp/pieeprom-new.bin /tmp/pieeprom-old.bin
    rpi-eeprom-digest -i /tmp/pieeprom-new.bin -o /tmp/pieeprom.sig
  "

cp /tmp/pieeprom-new.bin /firmware/pieeprom.upd
cp /tmp/pieeprom.sig     /firmware/pieeprom.sig
cp /tmp/recovery.bin     /firmware/recovery.bin
sync
'
```

Same firmware (`pieeprom-2025-05-08.bin`) as pi-desktop — avoid the
`raspberrypi-eeprom-2026.01.09` regression that breaks Pi 4 netboot.

### 8. Reboot — first real netboot

```sh
ssh root@10.0.4.<DHCP-IP> reboot
```

Pi boots once off USB → recovery.bin flashes the EEPROM → Pi self-
reboots → new `BOOT_ORDER=0xf42` puts NET first → DHCP + TFTP from
the cpi-N MAC dir → kernel + initrd load → initrd iSCSI mount → pivot
to the cpi-N closure.

Wait ~2-3 min, then `ssh ereslibre@10.0.4.<reserved-IP>`:

```sh
ssh ereslibre@10.0.4.<reserved-IP> '
hostname                       # should be cpi-N
readlink /run/current-system   # should be /nix/store/...-nixos-system-cpi-N-...
findmnt -no SOURCE /            # should be /dev/sdb (the iSCSI LUN)
'
```

### 9. Confirm pure netboot, then sops rekey

```sh
ssh root@10.0.4.<reserved-IP> poweroff
# Pull USB.
# Power back on. Same checks above should succeed without any USB present.
```

Then rekey sops with cpi-N's new host key (same pattern pi-desktop
went through):

```sh
ssh ereslibre@10.0.4.<reserved-IP> 'cat /etc/ssh/ssh_host_ed25519_key.pub' \
  | nix run nixpkgs#ssh-to-age
# Add the resulting age1... as `&host-cpi-N` recipient in .sops.yaml
# Add an entry under creation_rules for cpi-N/secrets.yaml
# Then create the secrets file or skip if no secrets needed yet.
nix run nixpkgs#sops -- updatekeys cpi-N/secrets.yaml
```

## Gotchas captured from cpi-1's first bring-up

- **`mkfs.ext4` default discard pass fills thin LUNs.** Without
  `-E nodiscard`, mkfs writes/UNMAPs every block of a fresh 128 GB
  LUN, which Synology then allocates fully (and won't release later
  if Space Reclamation wasn't enabled at LUN creation — see TODO).
  *Always* `mkfs.ext4 -E nodiscard -L PIROOT /dev/sdb`.
- **`nix-community.cachix.org`** is in `flake.nix` substituters
  specifically so emacs-overlay packages substitute instead of being
  built locally on an aarch64 Pi.
- **Build cpi-N's closure on hulk first**, then `nix copy` to the
  Pi. The installer's own `nixos-install` doesn't know about hulk as
  a builder; if you skip the pre-build, the Pi spends an hour-plus
  building emacs packages locally over the same Ethernet that iSCSI
  is writing through.
- **Killed `nixos-install` leaves zombie `nix build` children**
  holding `/mnt` store locks. The next install hangs at `0.0 KiB
  copied`. Look in `pgrep -af "nix build"` and `kill -9` the
  orphans.
- **`scp -O`** (legacy SCP protocol) for any Synology transfer —
  newer SFTP-based scp trips DSM's restricted shell.
- **Synology /tmp accumulates 0444 files from prior `cp -L` runs.**
  `scripts/deploy-tftp.sh` now `rm -f`'s them before scp. If you ever
  hit `scp: Permission denied` writing into Synology /tmp, that's
  what's happening — `rm` and retry.
- **EEPROM flash with `pieeprom-2025-05-08.bin`**, not the
  `raspberrypi-eeprom-2026.01.09` that nixpkgs currently ships as
  default — the 2026 firmware is silent on the wire on the Pi 4
  during netboot. See pi-desktop/README.md for the full story.
- **Installer USB sticks cross-contaminate EEPROMs.** A USB stick
  that previously had `pieeprom.upd` / `pieeprom.sig` /
  `recovery.bin` on its FAT partition (left over from a previous
  Pi's EEPROM flash) will silently re-flash the next Pi it's plugged
  into — with the *previous* Pi's config. Hit during the
  pi-desktop IQN rename on 2026-05-13: a cpi-N-prepped installer
  USB landed cpi-N's `TFTP_PREFIX=2` (MAC-based) onto pi-desktop.
  Hit again on 2026-05-14: a pi-desktop-prepped installer USB
  flashed pi-desktop's EEPROM config onto cpi-1 — and because the
  cpi-N fleet is PoE-powered, "pull ethernet to fall through to USB"
  was not an option for the recovery boot. The recovery in that case
  was to rename `/volume1/pis/pi-desktop` on the Synology so cpi-1's
  netboot failed fast and fell through to USB anyway.

  **Discipline**: keep the installer USB *permanently* free of those
  three files. Stage them onto the FAT partition only during step 7
  for the specific Pi being flashed, and `rm -f` them again before
  the USB ever leaves that Pi. The doc's step 7 already does the
  stage; the matching unstage is your responsibility before pulling
  the USB.
- **`nix copy` to the installer needs `--no-check-sigs`.** The
  homelab installer's nix-daemon refuses unsigned store paths from
  arbitrary remotes. Use
  `nix copy --no-check-sigs --to ssh-ng://root@<ip> <path>` when
  pushing a pre-built closure from hulk / a Mac to the installer.
- **`nixos-install --system <toplevel>` to skip flake re-eval.**
  If you've pre-built a closure and `nix copy`'d it to the
  installer, then `nixos-install --flake "github:…"` will *re-fetch*
  the flake from GitHub and re-evaluate — if the builder's local
  flake state differs from `origin/main` even slightly (uncommitted
  change, different lock), the eval produces different store paths
  and the Pi tries to build the gap (frequently OOMs on a 4 GB Pi).
  Pass the toplevel store path explicitly:
  `nixos-install --system /nix/store/<hash>-nixos-system-<host>-<ver> --no-bootloader --no-root-passwd --root /mnt`.
- **Pi 4 (4 GB) OOMs during nixos-install if it has to fetch or
  build anything.** Always pre-build on hulk (or a Mac with
  `nix.linux-builder`) and `nix copy` to the installer before
  invoking `nixos-install`. With the closure already local, the
  install is just a store copy from `/nix/store` to `/mnt/nix/store`
  and runs in seconds with negligible RAM.

## TODOs

### cpi-1 LUN is stuck at 128/128 GB allocated

cpi-1's LUN was created in DSM *without* "Enable Space Reclamation"
ticked. The `mkfs.ext4` discard pass therefore fully-allocated the
thin LUN, and DSM ignores subsequent `fstrim` / UNMAP requests so the
storage stays committed. cpi-1 functions fine, but ~115 GB of pool
space is wasted permanently.

Fix when convenient (~10 min, closure is already cached on hulk):

1. `ssh root@10.0.4.50 poweroff`
2. DSM SAN Manager → disconnect the iSCSI session → delete the
   `cpi-1-root` LUN
3. Recreate `cpi-1-root` LUN, 128 GB thin, **with "Enable Space
   Reclamation" ticked**
4. Bind to the existing
   `iqn.2000-01.com.synology:synology.cpi-1.ca49c4149b2` target
   (ACL stays as-is, allows the cpi-1 IQN)
5. Boot cpi-1 off the installer USB, redo iSCSI login + `mkfs.ext4
   -E nodiscard` + nixos-install (closure is cached on hulk, so the
   `nix copy --to ssh-ng://root@10.0.4.<DHCP>` is fast)
6. Repeat steps 6-9 from the bring-up procedure above
7. New cpi-1 host key → new sops rekey

### Rename targets so IQNs match host names

DSM auto-generates target IQN names from the target's display name,
and the early bring-ups left both hosts with opaque defaults:
pi-desktop was `default-target.<suffix>`, cpi-1 was `Target-1.<suffix>`.
Both have since been renamed to `<host>.<suffix>` to match host
names.

**Convention going forward**: name each target after the host it
serves (`pi-desktop`, `cpi-1`, `cpi-2`, …), so the IQN's last
identifier component is self-documenting:
`iqn.2000-01.com.synology:synology.<host>.<suffix>`. The procedure
below is kept as reference in case a future target is created with
an opaque name and needs the same fix.

**DSM does not let you rename an existing iSCSI target** — the name
is set at creation time and can only be changed by deleting and
recreating the target. Procedure for each host:

1. Power off the host (`ssh <host> poweroff`) so the running iSCSI
   session can be cleanly closed.
2. DSM SAN Manager → Sessions → disconnect any session attached to
   the old target.
3. DSM → Target → **unbind** the LUN from the old target (this leaves
   the LUN with its data intact) → **delete** the old target.
4. DSM → Target → **create new target named exactly `<host>`** (e.g.
   `pi-desktop`, `cpi-1`). DSM auto-generates a fresh IQN —
   `iqn.2000-01.com.synology:synology.<host>.<suffix>`. Copy that
   string.
5. **Bind the existing LUN** (`pi-desktop-root` / `cpi-1-root`) to
   the new target.
6. Set the new target's ACL to allow only the host's initiator IQN
   (`iqn.2026-04.net.ereslibre:<host>`).
7. Update the host's `hardware-configuration.nix` →
   `boot.iscsi-initiator.target` with the new IQN string. Commit
   and push.
8. Boot the host off its installer USB (since it can't netboot until
   the TFTP cmdline is refreshed) — or, if you're combining this with
   cpi-1's LUN-recreate TODO, do it in one go from the installer.
9. From hulk: `just deploy-tftp <host>` — picks up the new target
   IQN via the rebuilt closure and writes a fresh `cmdline.txt`.
10. Power-cycle. Pi netboots with the new IQN.

For **new** cpi-N targets going forward (cpi-2 onward): when
creating the target in DSM (step 1 of the bring-up procedure above),
name it exactly `cpi-N` from the start. No rename / delete-recreate
needed later.

### Reinstall onto an existing LUN (post-rename / recovery)

When the LUN already has data and you just need to refresh the
closure with new config (e.g. after an IQN rename, or recovering a
borked host), use this variant of the new-host bring-up — same
shape, but **don't `mkfs`** and use `nixos-install --system` to
avoid flake re-eval divergence.

```sh
# 1. From hulk (or a Mac with linux-builder): pre-build the closure.
just build <host>                            # ./result → /nix/store/<hash>-nixos-system-<host>-<ver>
TOPLEVEL=$(readlink -f ./result)
echo "$TOPLEVEL"                             # remember this path

# 2. Boot the Pi off the installer USB. If the host's existing TFTP
#    cmdline would loop in netboot (initrd hangs on the old iSCSI
#    target), force fall-through to USB. Two options depending on
#    how the Pi is powered:
#
#    - pi-desktop (separately powered): pull ethernet before power-
#      on, plug it back in once you're in the installer shell.
#    - cpi-N (PoE-powered, ethernet carries power): pulling ethernet
#      kills the Pi, so rename the host's TFTP dir on the Synology
#      *before* powering on instead:
#
#        ssh -t ereslibre@10.0.4.2 \
#          'sudo mv /volume1/pis/<host-tftp-dir> /volume1/pis/<host-tftp-dir>.HOLD'
#
#      Rename back after step 5 (`deploy-tftp`) has written the
#      fresh cmdline.

# 3. Push the closure to the installer Pi.
nix copy --no-check-sigs --to ssh-ng://root@<installer-ip> "$TOPLEVEL"

# 4. On the installer: log into the new iSCSI target, mount, install.
ssh root@<installer-ip> "
  set -e
  mkdir -p /etc/iscsi
  echo 'InitiatorName=iqn.2026-04.net.ereslibre:<host>' > /etc/iscsi/initiatorname.iscsi
  modprobe iscsi_tcp

  NIX_CONFIG='experimental-features = nix-command flakes' \
    nix shell nixpkgs#openiscsi --command bash -c '
      iscsid &
      sleep 2
      iscsiadm -m discovery -t st -p 10.0.4.2
      iscsiadm -m node -T <NEW-TARGET-IQN> -p 10.0.4.2 --login
    '
  sleep 2
  lsblk
  ls -l /dev/disk/by-label/PIROOT   # should symlink to /dev/sdb

  # *** DO NOT mkfs — LUN data is intact ***
  mkdir -p /mnt
  mount /dev/disk/by-label/PIROOT /mnt
  [ \"\$(findmnt -no SOURCE /mnt)\" = '/dev/sdb' ] || { echo WRONG DEVICE; exit 1; }

  nixos-install \
    --system $TOPLEVEL \
    --no-bootloader --no-root-passwd \
    --root /mnt

  umount /mnt

  NIX_CONFIG='experimental-features = nix-command flakes' \
    nix shell nixpkgs#openiscsi --command \
      iscsiadm -m node -T <NEW-TARGET-IQN> -p 10.0.4.2 --logout

  poweroff
"

# 5. Pull installer USB, plug ethernet, then from hulk:
just deploy-tftp <host>

# 6. Power on the Pi. Verify after it comes up:
ssh <host> 'cat /proc/cmdline; sudo iscsiadm -m session; readlink /run/current-system'
```

The `--system $TOPLEVEL` is what makes this safe: `nixos-install`
just copies the named closure into `/mnt/nix/store` and activates
— no flake fetch, no eval, no surprise rebuilds.
