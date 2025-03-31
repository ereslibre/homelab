{
  environment.shellAliases = {
    nixos-upgrade = ''sudo nixos-rebuild --refresh --flake "github:ereslibre/homelab#''${HOST}" switch'';
  };
}
