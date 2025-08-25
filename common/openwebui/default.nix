{
  services.open-webui = {
    enable = true;
    host = "0.0.0.0";
    environment = {
      OLLAMA_API_BASE_URL = "http://hulk.ereslibre.net:11434";
      WEBUI_AUTH = "False";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
    };
  };
}
