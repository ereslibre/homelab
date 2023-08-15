{
  username,
  profile,
  pkgs,
  ...
}: {
  home.file = {
    ".config/nix/nix.conf".source = ./assets/nix/nix.conf;
    ".emacs.d" = {
      source = ./assets/emacs/emacs.d;
      recursive = true;
    };
    ".gitconfig.vmware".source = ./assets/git/config-vmware;
    ".gitignore".source = ./assets/git/gitignore;
    ".gitconfig".source =
      if profile == "personal"
      then ./assets/git/full-config
      else ./assets/git/full-config-vmware;
    ".ssh/config".text = import ./assets/ssh/config.nix {inherit username;};
  };
}
