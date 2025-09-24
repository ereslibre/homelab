let
  rawHomeManagerConfigurations = {
    "ereslibre@hulk" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "23.05";
    };
    "ereslibre@nuc-1" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-2" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "22.11";
    };
    "ereslibre@nuc-3" = {
      system = "x86_64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = true;
      stateVersion = "23.05";
    };
    "ereslibre@devbox" = {
      system = "aarch64-linux";
      username = "ereslibre";
      homeDirectory = "/home/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      stateVersion = "25.05";
    };
    "ereslibre@Rafaels-Flying-Hulk" = {
      system = "aarch64-darwin";
      username = "ereslibre";
      homeDirectory = "/Users/ereslibre";
      profile = "personal";
      mainlyRemote = false;
      stateVersion = "25.05";
    };
  };
in {
  inherit rawHomeManagerConfigurations;
}
