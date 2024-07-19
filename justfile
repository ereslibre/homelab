defaultHost := "$(hostname -s)"

switch host=defaultHost:
  @./.switch.sh {{host}}

build host=defaultHost:
  nix build .#nixosConfigurations.{{host}}.config.system.build.toplevel

build-iso host=defaultHost:
  nix build --impure .#nixosConfigurations.{{host}}.config.system.build.isoImage

fmt:
  find . -name "*.nix" | xargs nix develop --command alejandra

ssh-to-age-key key="/etc/ssh/ssh_host_ed25519_key":
  sudo nix run nixpkgs#ssh-to-age -- -private-key -i {{key}} > ~/.config/sops/age/keys.txt

age-gen host=defaultHost:
  ssh-keyscan {{host}} | nix run nixpkgs#ssh-to-age

edit-secrets host=defaultHost:
  nix run nixpkgs#sops -- {{host}}/secrets.yaml
