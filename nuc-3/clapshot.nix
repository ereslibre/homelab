{...}: let
  # The all-in-one demo image (server + organizer + nginx in one container).
  # Upstream is explicit that it is for evaluation only; the production path is
  # the Compose recipes in deploy/compose/ or the .deb packages.
  # https://github.com/elonen/clapshot#demo
  #
  # Pinned by digest so a rebuild always gets the bits we tested against. The
  # tag is documentation only -- podman resolves the digest and ignores it --
  # so it names the release rather than the `latest-demo` alias that also
  # points here today. To bump, pick a tag and resolve it:
  #   skopeo inspect docker://docker.io/elonen/clapshot:<tag> | jq -r .Digest
  #
  # Fully qualified on purpose: this host's registries.conf has no
  # unqualified-search registries, so a short name fails to resolve at pull
  # time rather than defaulting to Docker Hub.
  image = "docker.io/elonen/clapshot:0.12.1-demo@sha256:beae6f1795bfc29a113b96e9f32ca92946385cae2d2d1ebf3637cb63d077e26d";

  # Everything stateful -- the SQLite database, the transcoded media, the
  # incoming/ drop directory and clapshot.log -- lives under this single
  # directory inside the container, so bind-mounting it from the host is all
  # that is needed for restarts and image bumps to be non-destructive.
  hostDataDir = "/var/lib/clapshot/data";
  containerDataDir = "/mnt/clapshot-data/data";

  port = 8083;

  # The entrypoint bakes absolute ws:// and upload URLs into the client config
  # from this value, so it has to be the hostname users actually type -- a
  # mismatch loads the page fine and then fails to open the websocket. Pointed
  # at MagicDNS because tailnet clients are the intended audience; CORS is
  # derived from it automatically.
  urlBase = "http://nuc-3.deer-nessie.ts.net:${toString port}/";
in {
  # The entrypoint runs `chown -R docker` (uid 1000, baked into the image at
  # build time) over the data dir on every start. Rootful podman maps uids 1:1,
  # so pre-create the host side with that ownership and the chown is a no-op
  # instead of a surprise on first boot.
  systemd.tmpfiles.rules = [
    "d /var/lib/clapshot 0755 1000 1000 -"
    "d ${hostDataDir} 0755 1000 1000 -"
  ];

  virtualisation.oci-containers = {
    # Both docker and podman are enabled on this host; podman is the module
    # default, but say so explicitly rather than leave it to whichever one the
    # option default happens to be.
    backend = "podman";

    containers.clapshot = {
      inherit image;
      autoStart = true;

      # No `pull` override on purpose: the default "missing" is what a digest
      # pin wants. Anything that re-checks the registry on start would buy
      # nothing (the digest cannot move) and make the unit depend on the
      # network to come up. Updates happen by editing the digest above.

      # Bound to all interfaces on purpose: reachable both over the LAN and
      # over tailscale0. nuc-3 runs with networking.firewall.enable = false
      # (common/node), so no port needs opening. Note the no-auth variant has
      # no login at all -- nginx rewrites every visitor to the single user
      # `docker` -- so anyone who can reach this port is that user.
      ports = ["0.0.0.0:${toString port}:80"];

      volumes = ["${hostDataDir}:${containerDataDir}"];

      environment = {
        CLAPSHOT_SERVER__URL_BASE = urlBase;
      };
    };
  };
}
