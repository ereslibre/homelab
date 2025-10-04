{
  services.wyoming = {
    faster-whisper.servers.turbo = {
      enable = true;
      device = "cuda";
      model = "turbo";
      language = "es";
      uri = "tcp://0.0.0.0:10300";
    };
    piper.servers = {
      davefx = {
        enable = true;
        voice = "es_ES-davefx-medium";
        uri = "tcp://0.0.0.0:10200";
        useCUDA = true;
      };
    };
  };
}
