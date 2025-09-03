defaultHost := "$(hostname -s)"

switch host=defaultHost: (build host)
  @./.switch.sh {{host}}

build host=defaultHost:
  nix build .#nixosConfigurations.{{host}}.config.system.build.toplevel

fmt:
  find . -name "*.nix" | xargs nix develop --command alejandra

ssh-to-age-key key="/etc/ssh/ssh_host_ed25519_key":
  sudo nix develop --command ssh-to-age -private-key -i {{key}} > ~/.config/sops/age/keys.txt

age-gen host=defaultHost:
  ssh-keyscan {{host}} | nix run nixpkgs#ssh-to-age

age-public-from-private key="~/.config/sops/age/keys.txt":
  cat {{key}} | nix develop --command age-keygen -y

cat-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --command sops decrypt {{host}}/secrets.yaml

edit-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --command sops -- {{host}}/secrets.yaml

rotate-secrets host=defaultHost:
  SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt nix develop --command sops -- rotate -i {{host}}/secrets.yaml

update-keys host=defaultHost:
  nix develop --command sops -- updatekeys {{host}}/secrets.yaml

update-emacs-d:
  scripts/update-emacs-d.sh
