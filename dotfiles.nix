{
  username,
  profile,
  pkgs,
}: let
  macFiles = import ./mac.nix {inherit username pkgs;};
in
  {
    ".config/nix/nix.conf".source = ./assets/nix/nix.conf;
    ".emacs.d" = {
      source = ./assets/emacs/emacs.d;
      recursive = true;
    };
    ".gitconfig".source =
      if profile == "personal"
      then ./assets/git/full-config
      else ./assets/git/full-config-vmware;
    ".gitconfig.vmware".source = ./assets/git/config-vmware;
    ".gitignore".source = ./assets/git/gitignore;
    ".ssh/config".text = import ./assets/ssh/config.nix {inherit username;};
  }
  // macFiles
