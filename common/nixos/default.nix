{
  nix = {
    extraOptions = "experimental-features = nix-command flakes";
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    settings.trusted-users = ["root" "builder" "ereslibre"];
  };
}
