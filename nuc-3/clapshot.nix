{config, ...}: let
  # The all-in-one demo image (server + organizer + nginx in one container).
  # Upstream is explicit that it is for evaluation only; the production path is
  # the Compose recipes in deploy/compose/ or the .deb packages.
  # https://github.com/elonen/clapshot#demo
  #
  # Our own build rather than upstream's docker.io/elonen/clapshot. The tag
  # names the git commit it was built from; the digest pin on top of it means a
  # rebuild always gets the bits we tested against even if the tag is ever
  # moved. To bump, build a new tag and resolve it (the package is private, so
  # this needs a token -- see the sops secret below):
  #   skopeo inspect docker://ghcr.io/ereslibre/clapshot:<tag> | jq -r .Digest
  #
  # Fully qualified on purpose: this host's registries.conf has no
  # unqualified-search registries, so a short name fails to resolve at pull
  # time rather than defaulting to Docker Hub.
  image = "ghcr.io/ereslibre/clapshot:git-2cc8a08-demo@sha256:6fccc47fc958d3a063b2ce73692a0b1b1fda5a05c364d3bacb0c310a1103c3d5";

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
  # ghcr.io/ereslibre/clapshot is a private package, so the pull needs
  # credentials. A classic PAT with only `read:packages` -- GHCR does not
  # accept fine-grained tokens for pulls, and nothing here needs write. Read
  # by the container unit's ExecStartPre as root, so the sops default of
  # root:root 0400 is what we want.
  sops.secrets."ghcr-pull-token" = {};

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

      # `podman login ghcr.io` in the unit's ExecStartPre, writing to root's
      # auth file so the `podman run` that follows can pull. The module makes
      # this best-effort: if the login fails it falls back to requiring the
      # image to already be present locally, so an expired token or a GitHub
      # outage does not stop an already-pulled clapshot from restarting.
      login = {
        registry = "ghcr.io";
        username = "ereslibre";
        passwordFile = config.sops.secrets."ghcr-pull-token".path;
      };

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
