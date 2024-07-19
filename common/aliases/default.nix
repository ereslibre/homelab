{
  environment.shellAliases = {
    nixos-upgrade = ''sudo nixos-rebuild --flake "github:ereslibre/homelab#''${HOST}" switch'';
  };
}
