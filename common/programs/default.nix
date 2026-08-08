{pkgs, ...}: {
  boot.loader.systemd-boot.memtest86.enable = true;
  programs = {
    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        libcap
      ];
    };
    zsh.enable = true;
  };
}
