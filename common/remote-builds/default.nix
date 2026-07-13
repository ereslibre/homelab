{config, ...}: {
  sops.secrets.hulk-builder-key.mode = "0400";

  programs.ssh.knownHosts = {
    hulk = {
      extraHostNames = ["hulk.ereslibre.net" "hulk.lab.ereslibre.local" "10.0.4.20"];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG7LVYEeuJKdK8ZGXwLGZxHaiCQNi107DdKl4CiJE8rC";
    };
  };

  nix = {
    buildMachines = [
      {
        sshUser = "builder";
        sshKey = config.sops.secrets.hulk-builder-key.path;
        # Use the FQDN, not the bare "hulk" name: the bare name resolves via
        # NSS/tailscale to hulk's tailscale IP on at least some clients,
        # routing build traffic over the overlay network instead of the LAN.
        hostName = "hulk.ereslibre.net";
        systems = ["x86_64-linux" "aarch64-linux"];
        protocol = "ssh-ng";
        supportedFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
        mandatoryFeatures = [];
        # hulk is a 64-core/128-thread Threadripper; without this it defaults
        # to 1, serializing every derivation onto a single remote build slot.
        maxJobs = 16;
      }
    ];
    distributedBuilds = true;
    settings.max-jobs = 0; # disable local builds; all derivations go to hulk
  };
}
