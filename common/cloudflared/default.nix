{
  config,
  pkgs,
  ...
}: {
  sops.secrets = {
    "ereslibre-social-cloudflare-tunnel.json" = {};
  };

  services.cloudflared = {
    enable = true;
    # Temporary: Cloudflare requires >= 2026.10.2; drop once nixos-unstable has it.
    package = pkgs.cloudflared.overrideAttrs (finalAttrs: _: {
      version = "2026.10.4";
      src = pkgs.fetchFromGitHub {
        owner = "cloudflare";
        repo = "cloudflared";
        tag = finalAttrs.version;
        hash = "sha256-HuKjytbGutwjUMepozeUmeeHczm4qI7vMvS+hgLsaZ0=";
      };
      vendorHash = "sha256-WtGk5HE6PxAWqwvj8mAAzobuoSw+y6BqBEEB0hRQafk=";
    });
    tunnels = {
      "5b97ef13-505f-41dc-82ff-b16cdce3a46b" = {
        credentialsFile = config.sops.secrets."ereslibre-social-cloudflare-tunnel.json".path;
        default = "http_status:404";
      };
    };
  };
}
