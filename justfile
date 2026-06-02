defaultHost := "$(hostname -s)"
configType := if `uname -s` == "Darwin" { "darwinConfigurations" } else { "nixosConfigurations" }

switch host=defaultHost: (build host)
  @./.switch.sh {{host}}

build host=defaultHost:
  nix build --accept-flake-config .#{{configType}}.{{host}}.config.system.build.toplevel

# Refresh the netboot tree for a TFTP-booted Pi host on the Synology.
# See scripts/deploy-tftp.sh for the per-host MAC / prefix mapping.
deploy-tftp host=defaultHost:
  scripts/deploy-tftp.sh {{host}}

# Build the SD-card installer image with our root ssh key baked in.
# Output is a .img.zst under ./result/sd-image/. dd it onto a USB stick
# / SD card, plug into a rack Pi, ssh root@<dhcp-ip>.
#
# `arch` selects the flake entry to build — needs a matching
# nixosConfigurations."<arch>-installer" in flake.nix (today: aarch64).
installer arch="aarch64":
  nix build --accept-flake-config .#nixosConfigurations.{{arch}}-installer.config.system.build.sdImage

fmt:
  find . -name "*.nix" | xargs nix develop --accept-flake-config --command alejandra

ssh-to-age-key key="/etc/ssh/ssh_host_ed25519_key":
  sudo nix develop --accept-flake-config --command ssh-to-age -private-key -i {{key}} > ~/.config/sops/age/keys.txt

age-gen host=defaultHost:
  ssh-keyscan {{host}} | nix run nixpkgs#ssh-to-age

age-public-from-private key="~/.config/sops/age/keys.txt":
  cat {{key}} | nix develop --accept-flake-config --command age-keygen -y

cat-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --accept-flake-config --command sops decrypt {{host}}/secrets.yaml

edit-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --accept-flake-config --command sops -- {{host}}/secrets.yaml

rotate-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --accept-flake-config --command sops -- rotate -i {{host}}/secrets.yaml

update-keys host=defaultHost:
  nix develop --accept-flake-config --command sops -- updatekeys {{host}}/secrets.yaml
