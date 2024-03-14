{
  username,
  profile,
}: {
  home.file = {
    ".config/nix/nix.conf".source = ./assets/nix/nix.conf;
    ".emacs.d" = {
      source = ./assets/emacs/emacs.d;
      recursive = true;
    };
    ".gitconfig.broadcom".source = ./assets/git/config-broadcom;
    ".gitignore".source = ./assets/git/gitignore;
    ".gitconfig".source =
      if profile == "personal"
      then ./assets/git/full-config
      else ./assets/git/full-config-broadcom;
    ".ssh/config".text = import ./assets/ssh/config.nix {inherit username;};
  };
}
