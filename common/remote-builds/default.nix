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
        hostName = "hulk";
        systems = ["x86_64-linux" "aarch64-linux"];
        protocol = "ssh-ng";
        supportedFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
        mandatoryFeatures = [];
      }
    ];
    distributedBuilds = true;
  };
}
