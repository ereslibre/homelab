{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  boot.loader.timeout = 0;

  environment = {
    etc."issue.d/ip.issue".text = "IPv4 address: \\4\nIPv6 address: \\6\n\n";
    systemPackages = with pkgs; [
      alacritty
      gtk4
      kitty
      mesa
    ];
  };

  hardware.graphics.enable = true;

  home-manager.users.ereslibre = {pkgs, ...}: {
    home.file.".config/hypr/hyprland.conf".source = ./assets/hyprland.conf;
  };

  networking = {
    dhcpcd.runHook = "${pkgs.utillinux}/bin/agetty --reload";
    hostName = "devbox";
    useDHCP = true;
  };

  programs = {
    hyprland.enable = true;
    zsh.enable = true;
  };

  services = {
    emacs.enable = true;
    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --remember --remember-session --asterisks --time";
          user = "greeter";
        };
        initial_session = {
          command = "hyprland";
          user = "ereslibre";
        };
      };
    };
    openssh.enable = true;
    spice-vdagentd.enable = true;
  };

  time.timeZone = "Europe/Madrid";

  users.users.ereslibre = {
    extraGroups = ["wheel"];
    isNormalUser = true;
    home = "/home/ereslibre";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDJz9rVLqUHt9ZFjep4RsN3B5xr9s6MtHSz4PbJHACj3bA3pP7UZwePzzDMofOZLhOIKzMJ+s9H0E28ruEN8xhAv9qPYN6DI15vvPoaMu4VbzyFOGAz4UXoMQpSkr3p9E8C3psJIMpgxOAGelp7PqODlCQS/6DVMqz3DqtkOJYPssAtivH1AfitA2NVPvI9bgswAhF0jArKJmnFPSy6DAc0G2q5DyVEZVfD943kOprd7GkVWdD9FpaHqjmLGd77RfHmmqrj9Tg5+ajYa+VrASJfTBkDJ/lZcFLH9DdfcUFcQzu2pzi/cX94e+FnTtog8TOGwCrWGDAZVPP5YEHmGU3QX0NQBl4vNprWe0oJaSjSzvIT2ZrixUhOWKTjW44To/+7UwIlCc4KWL/LJ+kbwWCpiOhhWqRs380cqUmuMRaq59uTIWCRImBTkqjTqBIxaj7060GV2ZWGzbYKhUsxPchx8KJVWyGxYoox+T/zQjF1KtwvnPVghIt4hiIifYCclxoeY1yAIU5T8LvZXaqBlSYPi715mJg7533IM6NhHMg09ANgkKt6fQmQUNtaYBpHfaIaKI68oSCJOFTiP3e1RYmKaz36GQPWqEBNKT5zaIYsSOMCyLhoecH6pF9Nqvust5iIpYgNSDlRh1qnOd1AUCimyJQiswsiEQTuCClbZHg152x33/6y8CZrpHRSzDh8cBApanvtQ5pmzD4IP9mZ3eGvWaSrVx6EtpYWkr4LSoPkh2dRWHdVu+a27TLVkl7V+2dE5WAIZzRsfpAfQB3JIVD5WmTVlbU1zgIIBSXr7SfGJo0bMQ59JptE9+ffoyGWk8fnbFww2re3QTphXau9Hy+88pUqvXkiYUxsSpHzXlpRAWbfR9wqCS3adKRaz+3vZYvJGP6d66ay9NRkTGeIKxEeYjdBSNues59UGsWiJVOaR9bxfvL5+F+WyIjv9a9yOJln9NXcADp32zUlAMY97+Kw0NRQeBnpX2fF6HjNLj+onlOt50EVNYGE3fS5CW8L9nSuIY4jAycQ8xF2GYG8lGgDfaCrGTVm0cFab6ytvLRFBaKFWqcIh2rYOgKV0p7qzoadQYH5hIH/V5LGt3yRgPdwqGHNd6662n5FKlio/omE1CUpAdedA1l+geMnaIdQpDG5ghjSb8jJnoUsPYVjTLQmg7g2HAnC2ofURbKAxEVfDDIuXmLp+plyb7DGGIhj6wprnoy7mDd/YJBzf9zmRjOz1mKhrgdbSHiDvfpbs0BW5HtodYHY7R6oEU8OtXYOR2bJfoqhspz5M/vmYBbzo5P7cbpBc5b6PW/xFnt2Sabuwrem0YTWh++eDmeDgSOK5F9k4NGQZriJYg5JqICqslht"
    ];
    hashedPassword = "$6$sRq3/7JWsvn46RNx$DC.k2MVs.xpQKKNNjg1yUUqXFYFvp7xD9dVp26PY/bfzGJeGE/zdvO7M6v83GcowXik0WlVuRyW2wHwF/0mqw/";
  };

  system = {
    stateVersion = "25.11";
    userActivationScripts.zshrc = "touch .zshrc";
  };

  # Guest disk size
  virtualisation.diskSize = 20480;
}
