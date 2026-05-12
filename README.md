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

`pi-desktop` is a Raspberry Pi 400 with no local storage that netboots
over TFTP and mounts root from an iSCSI LUN on the Synology. See
[`pi-desktop/README.md`](pi-desktop/README.md) for the full setup,
EEPROM flash recipe, and install procedure.

