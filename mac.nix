{username}: {pkgs, ...}: {
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
        <key>EnvironmentVariables</key>
        <dict>
          <key>XDG_RUNTIME_DIR</key>
          <string>/Users/${username}/.emacs.d</string>
        </dict>
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
  };
}
