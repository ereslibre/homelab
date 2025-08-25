{config, ...}: {
  sops.secrets."nix-access-tokens" = {
    owner = "ereslibre";
    group = "users";
  };

  nix.extraOptions = ''
    # Access tokens
    !include ${config.sops.secrets.nix-access-tokens.path}
  '';
}
