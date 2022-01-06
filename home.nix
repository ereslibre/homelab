{ config, pkgs, ... }: {
  home = {
    file = {
      ".bin/emacsclient" = {
        source = ./assets/emacs/emacsclient;
        executable = true;
      };
      # Make it conditional on Mac only
      "${config.home.homeDirectory}/Library/LaunchAgents/es.ereslibre.emacs.plist".text =
        ''
          <?xml version="1.0" encoding="UTF-8"?>
          <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
          <plist version="1.0">
          <dict>
            <key>Label</key>
            <string>es.ereslibre.es.emacs</string>
            <key>ProgramArguments</key>
            <array>
              <string>${pkgs.emacs}/bin/emacs</string>
              <string>--fg-daemon</string>
            </array>
            <key>RunAtLoad</key>
            <true/>
            <key>KeepAlive</key>
            <true/>
            <key>LSUIElement</key>
            <true/>
          </dict>
          </plist>
                '';
      ".bin/rosetta" = {
        source = ./assets/mac/rosetta;
        executable = true;
      };
      ".emacs.d" = {
        source = ./assets/emacs/emacs.d;
        recursive = true;
      };
      ".gitconfig".source = ./assets/git/config;
      ".gitconfig.suse".source = ./assets/git/config-suse;
      ".ssh/config".source = ./assets/ssh/config;
      ".tmux.conf".source = ./assets/tmux/config;
    };

    packages = import ./packages.nix { inherit pkgs; };
  };

  programs = {
    bash.enable = true;
    direnv.enable = true;
    emacs.enable = true;
    fzf.enable = true;
    keychain.enable = true;

    zsh = {
      enable = true;
      enableCompletion = false;
      envExtra = ''
        export PATH="${config.home.homeDirectory}/.bin:''${PATH}"
        export EDITOR="${pkgs.emacs}/bin/emacsclient -t"
        export LANG="en_US.UTF-8"
      '';
      initExtra = ''
        RPROMPT="$RPROMPT $(kubectx_prompt_info &> /dev/null)"

        token() {
          ${pkgs.yubikey-manager}/bin/ykman oath accounts code | grep -i "$1"
        }
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "kubectx" ];
        theme = "bira";
      };
      shellAliases = {
        dir = "dir --color=auto";
        emacs = "${pkgs.emacs}/bin/emacsclient -t";
        egrep = "egrep --color=auto";
        fgrep = "fgrep --color=auto";
        gpg = "${pkgs.gnupg}/bin/gpg --no-autostart";
        grep = "grep --color=auto";
        k = "${pkgs.kubectl}/bin/kubectl";
        l = "ls --color=auto -CF";
        ll = "ls --color=auto -alF";
        la = "ls --color=auto -A";
        ls = "ls --color=auto";
        vdir = "vdir --color=auto";
      };
    };
  };
}
