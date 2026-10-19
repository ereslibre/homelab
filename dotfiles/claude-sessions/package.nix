{
  lib,
  makeWrapper,
  rustPlatform,
  tmux,
}: let
  cargo = lib.importTOML ./Cargo.toml;
in
  rustPlatform.buildRustPackage {
    pname = cargo.package.name;
    inherit (cargo.package) version;

    src = lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.unions [./Cargo.toml ./Cargo.lock ./src];
    };
    cargoLock.lockFile = ./Cargo.lock;

    # Both subcommands shell out to tmux: the hook from inside Claude Code,
    # the server from a systemd user unit whose PATH has no tmux at all.
    nativeBuildInputs = [makeWrapper];
    postInstall = ''
      wrapProgram $out/bin/claude-sessions --prefix PATH : ${lib.makeBinPath [tmux]}
    '';

    meta = {
      inherit (cargo.package) description;
      license = lib.licenses.mit;
      mainProgram = "claude-sessions";
    };
  }
