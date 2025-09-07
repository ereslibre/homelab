{
  containers.vscode = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";

    bindMounts."/home/ereslibre".hostPath = "/home/ereslibre";

    config = {
      config,
      pkgs,
      ...
    }: {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [3000];
      };

      services.openvscode-server = {
        enable = true;
        host = "0.0.0.0";
        telemetryLevel = "off";
        withoutConnectionToken = true;
      };

      system.stateVersion = "25.05";
    };
  };
}
