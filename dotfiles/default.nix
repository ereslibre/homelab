let
  # cpi-N (cluster-pi) headless fleet — every home-manager entry is
  # identical except the host suffix. Edit `cpiNodes` to add/remove.
  cpiNodes = [1 2 3 4 5 6 7];
  mkCpiHM = n: {
    name = "ereslibre@cpi-${toString n}";
    value = {
      system = "aarch64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      aiTools = false;
      stateVersion = "26.05";
    };
  };
  cpiHomeManagerConfigurations = builtins.listToAttrs (map mkCpiHM cpiNodes);

  rawHomeManagerConfigurations = cpiHomeManagerConfigurations // {
    "ereslibre@hulk" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      aiTools = true;
      stateVersion = "23.05";
    };
    "ereslibre@nuc-1" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      aiTools = false;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-2" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      aiTools = false;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-3" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      aiTools = false;
      stateVersion = "23.05";
    };
    "ereslibre@pi-desktop" = {
      system = "aarch64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      aiTools = false;
      stateVersion = "23.05";
    };
    "ereslibre@Rafaels-Flying-Hulk" = {
      system = "aarch64-darwin";
      username = "ereslibre";
      homeDirectory = "/Users/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      aiTools = true;
      stateVersion = "25.05";
    };
  };
in {
  inherit rawHomeManagerConfigurations;
}
