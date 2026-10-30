{aiTools}: {
  llm-agents,
  pkgs,
  lib,
  ...
}: let
  ai-tools = lib.optionals aiTools (
    with llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      cursor-agent
      gemini-cli
      herdr
      skills
      qwen-code
    ]
  );
  # regclient 0.11.6 does not build at the current nixpkgs pin: its
  # postInstall ends by unsetting `outputBin`, which the multiple-outputs
  # fixup hook then dereferences as an empty name. Its tests are also
  # timing-sensitive and fail under qemu-user (how hulk builds the Pis),
  # so skip them on aarch64 only. Drop this override once a lock update
  # fixes the package.
  regctl =
    (pkgs.regclient.overrideAttrs (old: {
      postInstall = old.postInstall + "\noutputBin=out\n";
      doCheck = !pkgs.stdenv.hostPlatform.isAarch64;
    })).regctl;
  container-tools = with pkgs; ([dive reg regctl] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [distrobox]);
  core-tools = with pkgs; [
    binutils
    btop-cuda
    coreutils
    curl
    devenv
    diffutils
    dig
    file
    gnumake
    gnupg
    gnupg-pkcs11-scd
    just
    mosh
    mtr
    otpauth
    ripgrep
    rlwrap
    tree
    unixtools.netstat
    watch
    wget
    xxd
    zstd
  ];
  # rustup ships its own `rust-analyzer` proxy shim, but it only works if the
  # active toolchain has the rust-analyzer component installed (mutable ~/.rustup
  # state, not captured here) — otherwise it recurses on PATH and dies. Pull the
  # standalone nixpkgs rust-analyzer instead and give it hiPrio so it wins the
  # bin/rust-analyzer collision against rustup's shim. cargo/rustc still come
  # from rustup; project-local toolchains (devenv/.envrc) still override via PATH.
  global-language-tools = with pkgs; [gopls nodejs pnpm rustup (lib.hiPrio rust-analyzer)];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals pkgs.stdenv.hostPlatform.isLinux [k3d kind]));
  google-cloud-sdk = pkgs.google-cloud-sdk.withExtraComponents (with pkgs.google-cloud-sdk.components; [
    gke-gcloud-auth-plugin
  ]);
  platform-tools = with pkgs; [gh google-cloud-sdk];
  herdr-config = ''
    onboarding = false

    [keys]
    prefix = "ctrl+z"
    # Splitting and pane navigation as in the tmux config in programs.nix.
    split_vertical = ["prefix+v", "prefix+ctrl+v"]
    split_horizontal = ["prefix+s", "prefix+ctrl+s"]
    settings = "prefix+shift+s"
    focus_pane_left = ["prefix+h", "prefix+left"]
    focus_pane_down = ["prefix+j", "prefix+down"]
    focus_pane_up = ["prefix+k", "prefix+up"]
    focus_pane_right = ["prefix+l", "prefix+right"]
    cycle_pane_next = ["prefix+tab", "prefix+o"]
    open_notification_target = "prefix+shift+o"
    last_pane = "prefix+semicolon"
  '';
in {
  home.file.".config/herdr/config.toml" = lib.mkIf aiTools {text = herdr-config;};
  home.packages =
    ai-tools
    ++ container-tools
    ++ core-tools
    ++ global-language-tools
    ++ kubernetes-tools
    ++ platform-tools;
}
