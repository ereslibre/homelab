{home-manager, ...}: {
  lib,
  pkgs,
  ...
}: let
  hermesConfig = ./assets/hermes/config.yaml;
  hermesCurrentDatetimePlugin = ./assets/hermes/plugins/current-datetime;
in {
  # Copy hermes config to ~/.hermes/config.yaml as a mutable file so hermes
  # can write back runtime state. The marker file tracks the hash of the last
  # Nix-deployed version; the copy only runs when that hash changes (or on
  # first install), preserving any hermes-written changes in between switches.
  home.activation.hermesConfig = home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
    _hermes_src="${hermesConfig}"
    _hermes_dst="$HOME/.hermes/config.yaml"
    _hermes_marker="$HOME/.hermes/.nix-config-hash"
    _hermes_hash="$(${lib.getExe' pkgs.coreutils "sha256sum"} "$_hermes_src" | cut -d' ' -f1)"

    if [ ! -f "$_hermes_dst" ] || \
       [ ! -f "$_hermes_marker" ] || \
       [ "$(cat "$_hermes_marker")" != "$_hermes_hash" ]; then
      $DRY_RUN_CMD ${lib.getExe' pkgs.coreutils "install"} -Dm644 "$_hermes_src" "$_hermes_dst"
      $DRY_RUN_CMD ${pkgs.bash}/bin/sh -c "printf '%s' '$_hermes_hash' > '$_hermes_marker'"
    fi
  '';

  home.file.".hermes/plugins/current-datetime" = {
    source = hermesCurrentDatetimePlugin;
    recursive = true;
  };

  # Enabling linger makes the systemd user services start
  # automatically. In this machine, I want to trigger the
  # `gpg-forward-agent-path` service file automatically as
  # systemd starts, so the socket dir is always created and I
  # can forward the GPG agent through SSH directly without
  # having a first failed connection due to a missing
  # `/run/user/<id>/gnupg`.
  home.activation.linger = home-manager.lib.hm.dag.entryBefore ["reloadSystemd"] ''
    ${lib.getExe' pkgs.systemd "loginctl"} enable-linger $USER
  '';

  # Prevent gpg from auto-starting a local agent. On these
  # machines the GPG agent is forwarded over SSH from the
  # machine where the YubiKey is connected. Without this,
  # non-interactive callers (e.g. git) bypass the shell alias
  # and auto-start a local agent that has no smartcard access,
  # which squats on the socket and breaks signing even after
  # reconnecting SSH.
  home.file.".gnupg/gpg.conf".text = ''
    no-autostart
  '';

  # Disable the local GPG agent entirely. On node machines the
  # agent is forwarded over SSH, so a local agent must never
  # start — it would squat on the socket and shadow the
  # forwarded one. Setting extra-socket and browser-socket to
  # none prevents the agent from binding those sockets, and
  # no-allow-external-cache avoids keyring daemons caching
  # credentials locally.
  home.file.".gnupg/gpg-agent.conf".text = ''
    no-grab
    extra-socket none
    browser-socket none
    no-allow-external-cache
  '';

  programs = {
    bash.shellAliases = {
      gpg = "${lib.getExe' pkgs.gnupg "gpg"} --no-autostart";
    };
    zsh.shellAliases = {
      gpg = "${lib.getExe' pkgs.gnupg "gpg"} --no-autostart";
    };
  };
}
