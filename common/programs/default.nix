{pkgs, ...}: {
  programs = {
    nix-ld = {
      enable = true;

      # [rover] Fixes an issue when running tests on suites that download vscode binaries: [test:extension] /home/ereslibre/projects/endorhq/rover/packages/extension/.vscode-test/vscode-linux-x64-1.105.1/code: error while loading shared libraries: libglib-2.0.so.0: cannot open shared object file: No such file or directory
      libraries = with pkgs; [
        alsa-lib
        at-spi2-atk
        cairo
        dbus.lib
        expat
        glib
        gtk3
        libgbm
        libxkbcommon
        nspr
        nss_latest
        pango
        udev.dev
        xorg.libX11
        xorg.libxcb
        xorg.libXcomposite
        xorg.libXdamage
        xorg.libXext
        xorg.libXfixes
        xorg.libXrandr
      ];
    };

    zsh.enable = true;
  };
}
