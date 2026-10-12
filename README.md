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

## cpi-1 … cpi-7 (headless PoE Pi fleet)

Seven rack-mounted PoE-powered Pi 4 Bs. Each host is fully described
by its integer index via `mkCpi` in `flake.nix`; see
[`common/headless-pi/README.md`](common/headless-pi/README.md) for the
bring-up procedure.

