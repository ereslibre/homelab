{
  username,
  pkgs,
  ...
}: {
  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    extraFlags = ["--ignore-missing" "--quiet"];
    keys = ["id_ed25519" "id_rsa"];
  };
  home.file = {
    "Library/LaunchAgents/es.ereslibre.emacs.plist".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>Label</key>
        <string>es.ereslibre.es.emacs</string>
        <key>ProgramArguments</key>
        <array>
          <string>${pkgs.emacs}/bin/emacs</string>
          <string>--fg-daemon=/Users/${username}/.emacs.d/emacs.sock</string>
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
    ".config/alacritty/alacritty.yml".text = ''
      import:
        - ./dracula.yml
      env:
        TERM: xterm-direct
      font:
        normal:
          family: Fira Code
          style: Regular
        bold:
          family: Fira Code
          style: Bold
        italic:
          family: Fira Code
          style: Italic
        bold_italic:
          family: Fira Code
          style: Bold Italic
        size: 12
      shell:
        program: /Users/${username}/.nix-profile/bin/zsh
        args:
          - --login
      cursor:
        style:
          shape: Block
          blinking: Never
      window:
        decorations: full
        decorations_theme_variant: Dark
        dynamic_padding: true
        opacity: 0.9
        option_as_alt: Both
    '';
    ".config/alacritty/dracula.yml" = {
      source = ./assets/alacritty/dracula.yml;
    };
  };
}
