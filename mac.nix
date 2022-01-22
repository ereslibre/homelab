{ config, pkgs }:
if pkgs.stdenv.isDarwin then {
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
          <string>--fg-daemon=/Users/ereslibre/.emacs.d/emacs.sock</string>
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
} else
  { }
