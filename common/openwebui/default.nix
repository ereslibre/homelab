{
  containers.openwebui = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.14";
    localAddress = "192.168.100.15";

    config = {
      config,
      pkgs,
      ...
    }: {
      nixpkgs.config.allowUnfree = true;

      services.open-webui = {
        enable = true;
        environment = {
          OLLAMA_API_BASE_URL = "http://hulk.ereslibre.net:11434";
          WEBUI_AUTH = "False";
          ANONYMIZED_TELEMETRY = "False";
          DO_NOT_TRACK = "True";
          SCARF_NO_ANALYTICS = "True";
        };
      };

      system.stateVersion = "25.05";
    };
  };
}
