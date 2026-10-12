#!/usr/bin/env bash
# Stage the netboot tree for a Pi host on the Synology TFTP share.
#
# Two modes:
#   - Default: builds the host's closure with `nix build` (uses --refresh
#     so subsequent calls don't reuse a stale flake fetch) and stages
#     kernel/initrd/cmdline.txt from the resulting ./result symlink.
#   - --toplevel <path>: skips the build and uses an already-built
#     toplevel store path. Useful when running on the target Pi itself,
#     where the new generation lives at /run/current-system.
#
# Usage:
#   scripts/deploy-tftp.sh <hostname> [--toplevel <store-path>]
#
# Every cpi-* has its EEPROM flashed with TFTP_PREFIX=2, so the Pi
# fetches from /<mac-with-dashes>/: extend the MAC_OF map below when you
# add one. A host flashed with a literal prefix instead (TFTP_PREFIX=1,
# TFTP_PREFIX_STR=<dir>/) names its directory with TFTP_SUBDIR=<dir>.

set -euo pipefail

SYNOLOGY_USER="${SYNOLOGY_USER:-ereslibre}"
SYNOLOGY_HOST="${SYNOLOGY_HOST:-10.0.4.2}"
TFTP_BASE="${TFTP_BASE:-/volume1/pis}"

declare -A MAC_OF=(
  [cpi-1]=dc:a6:32:b1:07:03
  [cpi-2]=dc:a6:32:b1:06:f7
  [cpi-3]=e4:5f:01:1b:25:31
  [cpi-4]=dc:a6:32:e2:1f:14
  [cpi-5]=e4:5f:01:1b:25:eb
  [cpi-6]=dc:a6:32:b1:06:e3
  [cpi-7]=dc:a6:32:b1:06:df
)

die() { echo "deploy-tftp: $*" >&2; exit 1; }

[[ $# -ge 1 ]] || die "usage: deploy-tftp.sh <hostname> [--toplevel <path>]"

HOST=$1
shift

TOPLEVEL=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --toplevel) TOPLEVEL=$2; shift 2 ;;
    *) die "unknown flag: $1" ;;
  esac
done

# Resolve TFTP destination subdir for this host.
TFTP_SUBDIR="${TFTP_SUBDIR:-}"
if [[ -z "$TFTP_SUBDIR" ]]; then
  [[ -n "${MAC_OF[$HOST]:-}" ]] ||
    die "no MAC_OF entry for host '$HOST' — add one to scripts/deploy-tftp.sh, or set TFTP_SUBDIR"
  TFTP_SUBDIR=${MAC_OF[$HOST]//:/-}
fi

echo "deploy-tftp: host=$HOST  tftp_subdir=$TFTP_SUBDIR"

# Build closure if no --toplevel was passed.
if [[ -z "$TOPLEVEL" ]]; then
  echo "deploy-tftp: nix-building toplevel for $HOST (may take a while)"
  nix build --accept-flake-config --refresh \
    ".#nixosConfigurations.${HOST}.config.system.build.toplevel" \
    --print-out-paths --no-link > /tmp/deploy-tftp-toplevel
  TOPLEVEL=$(< /tmp/deploy-tftp-toplevel)
fi

[[ -d "$TOPLEVEL" ]] || die "toplevel not a directory: $TOPLEVEL"
[[ -e "$TOPLEVEL/kernel" && -e "$TOPLEVEL/initrd" && -e "$TOPLEVEL/kernel-params" ]] \
  || die "toplevel missing kernel/initrd/kernel-params: $TOPLEVEL"

echo "deploy-tftp: toplevel=$TOPLEVEL"

# Stage locally.
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT

cp -L "$TOPLEVEL/kernel" "$STAGE/kernel8.img"
cp -L "$TOPLEVEL/initrd" "$STAGE/initrd"

KERNEL_PARAMS=$(< "$TOPLEVEL/kernel-params")
cat > "$STAGE/cmdline.txt" <<EOF
init=$TOPLEVEL/init $KERNEL_PARAMS ip=dhcp root=/dev/disk/by-label/PIROOT rootfstype=ext4 rootwait
EOF

echo "deploy-tftp: cmdline.txt:"
sed 's/^/  /' "$STAGE/cmdline.txt"

# Push to Synology via /tmp staging because (a) DSM scp lands in a
# restricted dir, and (b) /volume1/pis is root-owned. scp -O forces the
# legacy SCP protocol, which DSM accepts; the new SFTP-based scp fails
# with "remote mkdir / Permission denied" on DSM.
echo "deploy-tftp: scp -O staged files to ${SYNOLOGY_USER}@${SYNOLOGY_HOST}:/tmp/"
# `cp -L` copies the read-only bit from the Nix store, so the previous
# run leaves /tmp/{kernel8.img,initrd,cmdline.txt} as 0444 — scp can't
# then overwrite them on the next deploy. Force-remove first.
ssh "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" 'rm -f /tmp/kernel8.img /tmp/initrd /tmp/cmdline.txt'
for f in kernel8.img initrd cmdline.txt; do
  scp -O "$STAGE/$f" "${SYNOLOGY_USER}@${SYNOLOGY_HOST}:/tmp/$f"
done

DEST="$TFTP_BASE/$TFTP_SUBDIR"
echo "deploy-tftp: sudo mv into $DEST (you will be prompted for the Synology sudo password)"
ssh -t "${SYNOLOGY_USER}@${SYNOLOGY_HOST}" "
  set -eu
  sudo mkdir -p '$DEST'
  sudo mv /tmp/kernel8.img /tmp/initrd /tmp/cmdline.txt '$DEST/'
  echo '--- $DEST contents:'
  sudo ls -la '$DEST/' | head -20
  echo '--- cmdline.txt:'
  sudo cat '$DEST/cmdline.txt'
"
