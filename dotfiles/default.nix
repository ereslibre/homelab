let
  rawHomeManagerConfigurations = {
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
