# pi-desktop netboot — in-progress state

Working through `README.md` "pi-desktop (diskless iSCSI netboot)" with Claude.
This file is a session handoff — delete (or commit) once boot works.

## Where we are right now (2026-04-25, ~22:55)

**Currently running**: `nixos-install` on the Pi, writing the pi-desktop
closure to the iSCSI LUN. Backgrounded as task `bqpwxr21x` on hulk.
Should take 10-30 min depending on cache hits and Synology iSCSI write
throughput.

The install has cleared flake evaluation and is into the substituter
copy phase (`copying path '/nix/store/...source' from cache.nixos.org`).
Progress is monitorable via `tail -f /tmp/pi-nixos-install.log` on hulk
and `df -h /volume1` on the Synology (LUN is thin-provisioned — Used on
/volume1 grows as the closure lands).

## Topology (machines and their roles)

- **hulk** (10.0.4.20) — this workstation; driver. Runs:
  - `python3 -m http.server 8081 --bind 0.0.0.0` in `/tmp` for the Pi
    to fetch helper scripts.
  - Persistent SSH session (via `sshpass`) into the Pi running
    `nixos-install`.
- **pi-desktop** (10.0.4.40) — Pi 400 booted off an external **USB SSD**
  running the NixOS aarch64 minimal installer. The Pi 400's built-in
  SD reader is physically broken (auto-ejects); USB only.
  - `iscsid` running, logged into the Synology target.
  - `/dev/sda` = USB SSD (931 GB, the live installer's rootfs).
  - `/dev/sdb` = iSCSI LUN (128 GB), formatted ext4 with label `PIROOT`,
    mounted at `/mnt`.
- **Synology DS923+** (10.0.4.2) — storage + future TFTP boot server.
  - LUN `pi-desktop-root`, IQN
    `iqn.2000-01.com.synology:synology.default-target.ca49c4149b2`,
    UUID `da8cc6c6-a987-4324-b304-a653fa6449e2`, 128 GiB thin.
  - Backing file lives under `/volume1/@iSCSI/` (root-only access).
  - TFTP root `/volume1/pis/`. Boot tree staged in
    `/volume1/pis/pi-desktop/`: `bootcode.bin`, `start4.elf`,
    `fixup4.dat`, `bcm2711-rpi-400.dtb`, `bcm2711-rpi-4-b.dtb`,
    `overlays/`, `kernel8.img` (32 MB), `initrd` (12 MB), `cmdline.txt`,
    `config.txt`.

## DHCP / network

- DSM DHCP not used (intentional). Wired-lab DHCP at 10.0.4.1 hands the
  Pi 10.0.4.40. Static reservation still pending (need the Pi's `end0`
  MAC; will grab once it's booted into the real config).
- Pi 4 EEPROM has `TFTP_IP=10.0.4.2` baked in (planned), so DHCP option
  66 / next-server / option 43 are not required.

## Code changes pushed this session

Branch `pi-desktop-netboot-fix` (commit `b183f6b7`), pushed to
`origin/pi-desktop-netboot-fix`. Three substantive fixes:

- `pi-desktop/hardware-configuration.nix`:
  - `target` IQN updated to the DSM-auto-generated string.
  - `boot.initrd.systemd.enable = false` pinned (modern nixpkgs default
    flipped to `true`, but `boot.iscsi-initiator` requires the legacy
    bash-script initrd).
- `pi-desktop/configuration.nix`:
  - `networking.useDHCP = true` removed — collided with NetworkManager
    (auto-enabled by GNOME). NM manages `end0` post-pivot; the initrd
    has its own DHCP.
- `README.md`:
  - TFTP root corrected to `/volume1/pis/pi-desktop/`
    (`TFTP_PREFIX=0` + `TFTP_PREFIX_STR=pi-desktop`).
  - "First install into the LUN" rewritten to use a USB-SD-card
    bootstrap (Mac builds the SD image, Pi runs the install).
  - "Pi EEPROM" section: `BOOT_ORDER=0xf24` (NETWORK → USB → stop) with
    LSB-first nibble explanation. Notes the SD slot is broken on this
    Pi.

Commit was `--no-gpg-sign` (gpg-agent wasn't running in the shell —
explicitly requested by the user).

## Uncommitted (still local)

- `pi-desktop/NETBOOT_PROGRESS.md` (this file).
- The Mac side has a series of edits the user made on top of
  `pi-desktop-netboot-fix` to gate `nix-ai-tools` behind a per-host
  `aiTools` flag (`dotfiles/default.nix`, `dotfiles/home.nix`,
  `dotfiles/packages.nix`, `flake.nix`). Not yet pushed. Required for
  pi-desktop to build without pulling codex via the private R2
  substituter (which was unreachable during the Spanish ISP's
  match-time Cloudflare blocks).

## What worked / what blew up along the way

- Built once on hulk under `binfmt-aarch64` emulation — slow + module
  errors. Pivoted to building on the Mac (native aarch64 via
  `nix.linux-builder`).
- First Mac build failed: TLS-intercepted Cloudflare R2 substituter
  (`core1.netops.test` — Spanish LaLiga IP block). Resolved by user
  enabling NordVPN + the `aiTools` gating change.
- Pi installer doesn't ship `iscsi-initiator-utils`. Bootstrapped via
  `nix-shell -p openiscsi --command bash -c '...'` (had to enable
  `experimental-features = nix-command flakes` via `NIX_CONFIG`).
- First `nixos-install` attempt: `mount /dev/disk/by-label/PIROOT`
  failed silently (udev hadn't refreshed the by-label symlink right
  after `mkfs`). The script lacked `set -e`, so `nixos-install` ran
  with `/mnt` = the live installer's own rootfs. Aborted; remounted
  by device path (`mount /dev/sdb /mnt`); guarded the retry with
  `[ "$(findmnt -no SOURCE /mnt)" = /dev/sdb ]`.

## Memory entries created

- `feedback_hulk_is_local.md` — never `ssh hulk` from inside this repo.
- `project_spain_laliga_blocks.md` — Cloudflare TLS errors with cert
  `core1.netops.test` are the ISP, not the network.
- `project_endor_sd_reader_broken.md` — pi-desktop's SD slot is broken;
  EEPROM `BOOT_ORDER` must skip the SD nibble.

## Task tracker (Claude task tool, last snapshot)

| # | Status        | Subject |
|---|---------------|---------|
| 1 | completed     | DSM: create LUN, target & ACL |
| 2 | completed     | DSM: enable TFTP server |
| 3 | completed     | Build pi-desktop closure on hulk *(actually done on Mac)* |
| 4 | completed     | Stage TFTP payload to Synology |
| 5 | in_progress   | Install NixOS into the iSCSI LUN |
| 6 | pending       | Flash Pi EEPROM for net boot |
| 7 | pending       | Boot test + re-key sops |

## To resume

1. Wait for `nixos-install` to finish. Watch
   `tail -f /tmp/pi-nixos-install.log` on hulk.
2. On success, on the Pi:
   ```sh
   umount /mnt
   iscsiadm -m node -T iqn.2000-01.com.synology:synology.default-target.ca49c4149b2 -p 10.0.4.2 --logout
   poweroff
   ```
3. Pull the USB SSD.
4. EEPROM step (Step 6 in README): boot the Pi off another USB drive
   running Raspberry Pi OS (since SD slot is broken), run
   `rpi-eeprom-config --edit`, set the values from the README EEPROM
   section, save, reboot.
5. Pull the second USB; the next cold boot is the first real netboot.
6. Step 7: re-key sops with the Pi's new host key.

## Side issues to clean up later

- Two evaluation warnings to fix (NixOS 26.05+):
  - `services.xserver.desktopManager.gnome.enable` →
    `services.desktopManager.gnome.enable`
  - `services.xserver.displayManager.gdm.enable` →
    `services.displayManager.gdm.enable`
- "Scripted initrd is deprecated and scheduled for removal in 26.11"
  — but `boot.iscsi-initiator` still requires it. Track upstream
  (nixpkgs) for systemd-stage-1 iSCSI support; switch back to
  `boot.initrd.systemd.enable = true` once available, otherwise
  pi-desktop will need a custom initrd by 26.11.
- Static DHCP reservation for the Pi's `end0` MAC at 10.0.4.1.
- Old `host-pi-desktop` age recipient in `.sops.yaml` is stale; re-key
  per README "Re-key sops after first boot".
- Mac-side `aiTools` flag changes are uncommitted on the Mac; should be
  folded into `pi-desktop-netboot-fix` (or merged separately) so other
  hosts don't pull codex unnecessarily.
- Background processes still running on hulk: a python http.server on
  port 8081 (task `baqdbi36l`). Kill once the install is done.
