{config, ...}: {
  sops.secrets = {
    "ereslibre-social-cloudflare-tunnel.json" = {};
  };

  services.cloudflared = {
    enable = true;
    tunnels = {
      "5b97ef13-505f-41dc-82ff-b16cdce3a46b" = {
        credentialsFile = config.sops.secrets."ereslibre-social-cloudflare-tunnel.json".path;
        default = "http_status:404";
      };
    };
  };
}
