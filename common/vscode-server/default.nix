{
  services.openvscode-server = {
    enable = true;
    host = "0.0.0.0";
    telemetryLevel = "off";
    withoutConnectionToken = true;
  };
}
