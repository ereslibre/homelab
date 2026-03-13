{pkgs, ...}: {
  programs = {
    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        glib
        libcap
      ];
    };
    zsh.enable = true;
  };
}
