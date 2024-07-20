{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../common/aliases
    ../common/nixos
    ../common/packages
    ../common/podman
    ../common/programs
    ../common/services
    ../common/tailscale
    ../common/users
  ];

  # Temporary due to spice-autorandr
  nixpkgs.config.allowUnsupportedSystem = true;

  environment.variables = {
    TERMINAL = "terminator";
  };

  fonts = {
    fontconfig.defaultFonts = {
      serif = [
        "DejaVu Sans"
      ];
      sansSerif = [
        "DejaVu Sans"
      ];
      monospace = [
        "Fira Code"
      ];
    };
    packages = with pkgs; [
      dejavu_fonts
      fira-code
      iosevka
      ubuntu_font_family
    ];
  };

  home-manager.users.ereslibre = {
    home.pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11.enable = true;
      gtk.enable = true;
      size = 48;
    };
    programs = {
      alacritty.settings.font.size = lib.mkForce 7;
      emacs.extraConfig = ''
        (set-frame-font "Iosevka-10:Regular")
        (add-to-list 'default-frame-alist '(font . "Iosevka-10:Regular"))
        (set-face-attribute 'default t :font "Iosevka-10:Regular")
      '';
      firefox.enable = true;
      terminator = {
        enable = true;
        config = {
          global_config = {
            tab_position = "hidden";
            # Dracula
            title_transmit_fg_color = "#282a36";
            title_transmit_bg_color = "#50fa7b";
            title_receive_fg_color = "#282a36";
            title_receive_bg_color = "#ff79c6";
            title_inactive_fg_color = "#f8f8f2";
            title_inactive_bg_color = "#44475a";
            inactive_color_offset = "0.61";
            suppress_multiple_term_dialog = true;
            title_hide_sizetext = true;
            # !Dracula
          };
          profiles = {
            default = {
              use_system_font = false;
              font = "Fira Code 10";
              cursor_blink = false;
              scrollbar_position = "disabled";
              show_titlebar = false;
              # Dracula
              background_color = "#282a36";
              background_image = "None";
              foreground_color = "#f8f8f2";
              palette = "#262626:#e356a7:#42e66c:#e4f34a:#9b6bdf:#e64747:#75d7ec:#efa554:#7a7a7a:#ff79c6:#50fa7b:#f1fa8c:#bd93f9:#ff5555:#8be9fd:#ffb86c";
              # !Dracula
            };
          };
        };
      };
    };
    gtk.enable = true;
    xsession.enable = true;
  };

  networking = {
    hostName = "devbox";
    wireless.enable = false;
  };

  services = {
    xserver = {
      dpi = 192;
      upscaleDefaultCursor = true;
    };
    displayManager = {
      defaultSession = "none+i3";
      autoLogin = {
        enable = true;
        user = "ereslibre";
      };
    };
    libinput = {
      touchpad.naturalScrolling = true;
      mouse.naturalScrolling = true;
    };
    spice-autorandr.enable = true;
    spice-vdagentd.enable = true;
    qemuGuest.enable = true;
    xserver = {
      enable = true;
      windowManager.i3 = {
        enable = true;
        configFile = pkgs.writeText "i3-config" ''
          # i3 config file (v4)
          #
          # Please see https://i3wm.org/docs/userguide.html for a complete reference!
          #
          # This config file uses keycodes (bindsym) and was written for the QWERTY
          # layout.
          #
          # To get a config file with the same key positions, but for your current
          # layout, use the i3-config-wizard
          #

          set $mod Mod1

          mode "passthrough" {
                  bindsym Mod4+semicolon mode "default"
          }
          bindsym Mod4+semicolon mode "passthrough"

          # Font for window titles. Will also be used by the bar unless a different font
          # is used in the bar {} block below.
          font xft:Fira Code Retina 10

          # Start XDG autostart .desktop files using dex. See also
          # https://wiki.archlinux.org/index.php/XDG_Autostart
          exec --no-startup-id dex --autostart --environment i3

          # The combination of xss-lock, nm-applet and pactl is a popular choice, so
          # they are included here as an example. Modify as you see fit.

          # xss-lock grabs a logind suspend inhibit lock and will use i3lock to lock the
          # screen before suspend. Use loginctl lock-session to lock your screen.
          exec --no-startup-id xss-lock --transfer-sleep-lock -- i3lock --nofork

          # NetworkManager is the most popular way to manage wireless networks on Linux,
          # and nm-applet is a desktop environment-independent system tray GUI for it.
          exec --no-startup-id nm-applet

          # Use pactl to adjust volume in PulseAudio.
          set $refresh_i3status killall -SIGUSR1 i3status
          bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status
          bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status
          bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status
          bindsym XF86AudioMicMute exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status

          # use these keys for focus, movement, and resize directions when reaching for
          # the arrows is not convenient
          set $up l
          set $down k
          set $left j
          set $right semicolon

          # use Mouse+Mod1 to drag floating windows to their wanted position
          floating_modifier $mod

          # move tiling windows via drag & drop by left-clicking into the title bar,
          # or left-clicking anywhere into the window while holding the floating modifier.
          tiling_drag modifier titlebar

          # start a terminal
          bindsym $mod+Return exec i3-sensible-terminal
          # start editor
          bindsym $mod+m exec emacs

          # kill focused window
          bindsym $mod+Shift+q kill

          # start dmenu (a program launcher)
          bindsym $mod+d exec --no-startup-id dmenu_run -fn 'FiraCode-10 -h 24'
          # A more modern dmenu replacement is rofi:
          # bindsym $mod+d exec "rofi -modi drun,run -show drun"
          # There also is i3-dmenu-desktop which only displays applications shipping a
          # .desktop file. It is a wrapper around dmenu, so you need that installed.
          # bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

          # change focus
          bindsym $mod+$left focus left
          bindsym $mod+$down focus down
          bindsym $mod+$up focus up
          bindsym $mod+$right focus right

          # alternatively, you can use the cursor keys:
          bindsym $mod+Left focus left
          bindsym $mod+Down focus down
          bindsym $mod+Up focus up
          bindsym $mod+Right focus right

          # move focused window
          bindsym $mod+Shift+$left move left
          bindsym $mod+Shift+$down move down
          bindsym $mod+Shift+$up move up
          bindsym $mod+Shift+$right move right

          # alternatively, you can use the cursor keys:
          bindsym $mod+Shift+Left move left
          bindsym $mod+Shift+Down move down
          bindsym $mod+Shift+Up move up
          bindsym $mod+Shift+Right move right

          # split in horizontal orientation
          bindsym $mod+h split h

          # split in vertical orientation
          bindsym $mod+v split v

          # enter fullscreen mode for the focused container
          bindsym $mod+f fullscreen toggle

          # change container layout (stacked, tabbed, toggle split)
          bindsym $mod+s layout stacking
          bindsym $mod+w layout tabbed
          bindsym $mod+e layout toggle split

          # toggle tiling / floating
          bindsym $mod+Shift+space floating toggle

          # change focus between tiling / floating windows
          bindsym $mod+space focus mode_toggle

          # focus the parent container
          bindsym $mod+a focus parent

          # focus the child container
          #bindsym $mod+d focus child

          # move the currently focused window to the scratchpad
          bindsym $mod+Shift+minus move scratchpad

          # Show the next scratchpad window or hide the focused scratchpad window.
          # If there are multiple scratchpad windows, this command cycles through them.
          bindsym $mod+minus scratchpad show

          # Define names for default workspaces for which we configure key bindings later on.
          # We use variables to avoid repeating the names in multiple places.
          set $ws1 "1"
          set $ws2 "2"
          set $ws3 "3"
          set $ws4 "4"
          set $ws5 "5"
          set $ws6 "6"
          set $ws7 "7"
          set $ws8 "8"
          set $ws9 "9"
          set $ws10 "10"

          # switch to workspace
          bindsym $mod+1 workspace number $ws1
          bindsym $mod+2 workspace number $ws2
          bindsym $mod+3 workspace number $ws3
          bindsym $mod+4 workspace number $ws4
          bindsym $mod+5 workspace number $ws5
          bindsym $mod+6 workspace number $ws6
          bindsym $mod+7 workspace number $ws7
          bindsym $mod+8 workspace number $ws8
          bindsym $mod+9 workspace number $ws9
          bindsym $mod+0 workspace number $ws10

          # move focused container to workspace
          bindsym $mod+Shift+1 move container to workspace number $ws1
          bindsym $mod+Shift+2 move container to workspace number $ws2
          bindsym $mod+Shift+3 move container to workspace number $ws3
          bindsym $mod+Shift+4 move container to workspace number $ws4
          bindsym $mod+Shift+5 move container to workspace number $ws5
          bindsym $mod+Shift+6 move container to workspace number $ws6
          bindsym $mod+Shift+7 move container to workspace number $ws7
          bindsym $mod+Shift+8 move container to workspace number $ws8
          bindsym $mod+Shift+9 move container to workspace number $ws9
          bindsym $mod+Shift+0 move container to workspace number $ws10

          # reload the configuration file
          bindsym $mod+Shift+c reload
          # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
          bindsym $mod+Shift+r restart
          # exit i3 (logs you out of your X session)
          bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'"

          # resize window (you can also use the mouse for that)
          mode "resize" {
                  # These bindings trigger as soon as you enter the resize mode

                  # Pressing left will shrink the window’s width.
                  # Pressing right will grow the window’s width.
                  # Pressing up will shrink the window’s height.
                  # Pressing down will grow the window’s height.
                  bindsym $left       resize shrink width 10 px or 10 ppt
                  bindsym $down       resize grow height 10 px or 10 ppt
                  bindsym $up         resize shrink height 10 px or 10 ppt
                  bindsym $right      resize grow width 10 px or 10 ppt

                  # same bindings, but for the arrow keys
                  bindsym Left        resize shrink width 10 px or 10 ppt
                  bindsym Down        resize grow height 10 px or 10 ppt
                  bindsym Up          resize shrink height 10 px or 10 ppt
                  bindsym Right       resize grow width 10 px or 10 ppt

                  # back to normal: Enter or Escape or $mod+r
                  bindsym Return mode "default"
                  bindsym Escape mode "default"
                  bindsym $mod+r mode "default"
          }

          bindsym $mod+r mode "resize"

          # Start i3bar to display a workspace bar (plus the system information i3status
          # finds out, if available)
          bar {
                  status_command i3status
          }
        '';
        extraSessionCommands = ''
          xrandr --output Virtual-1 --primary --mode 3840x2160 --dpi 192
          xset 26/10 4
          ${pkgs.feh}/bin/feh --bg-scale ${./wallpapers/nix-wallpaper-dracula.png}
        '';
      };
      displayManager.lightdm.enable = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
