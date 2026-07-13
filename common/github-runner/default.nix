{config, ...}: {
  # Organization-wide self-hosted GitHub Actions runner.
  #
  # Registers this host against the `curriedsoftware` org. The PAT stored
  # in sops (see `github-runner-pat` in the host's secrets.yaml) is used
  # by the module to fetch short-lived registration tokens and to
  # (re)register the runner on every deploy — so nothing to rotate by
  # hand. The PAT is a fine-grained token scoped to the org with
  # "Self-hosted runners" read & write permission.
  #
  # The service runs as a DynamicUser; the token is handed to it via a
  # systemd LoadCredential, so the sops secret can stay root:root 0400.
  sops.secrets."github-runner-pat" = {};

  services.github-runners.${config.networking.hostName} = {
    enable = true;
    name = config.networking.hostName;
    url = "https://github.com/curriedsoftware";
    tokenFile = config.sops.secrets."github-runner-pat".path;
    # Re-register (rather than fail) if a stale runner with this name
    # already exists on the org — keeps redeploys idempotent.
    replace = true;
    extraLabels = ["nixos" "homelab"];
  };
}
