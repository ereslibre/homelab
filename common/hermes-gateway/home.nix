# Home-manager side of the hermes gateway: everything that has to live
# in `~/.hermes` for the containerized gateway to pick it up. The
# container bind-mounts the host home directory (see ./default.nix), so
# this state is written on the host by home-manager and read by hermes
# inside the container.
#
# This is imported from ./default.nix rather than from the generic
# dotfiles, so it only lands on the hosts that actually run the gateway.
{
  lib,
  pkgs,
  ...
}: let
  hermesConfig = ./assets/config.yaml;
  hermesCurrentDatetimePlugin = ./assets/plugins/current-datetime;
  hermesNixomaticSkill = ./assets/skills/nixomatic;
  hermesGoogleWorkspaceSkill = ./assets/skills/google-workspace;
in {
  # Copy hermes config to ~/.hermes/config.yaml as a mutable file so hermes
  # can write back runtime state. The marker file tracks the hash of the last
  # Nix-deployed version; the copy only runs when that hash changes (or on
  # first install), preserving any hermes-written changes in between switches.
  home.activation.hermesConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
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

  # Hermes configuration files
  #
  # Our skills live in `~/.hermes/skills-external`, *not* in the
  # `~/.hermes/skills` tree hermes manages itself. Hermes re-seeds its
  # bundled skills into `~/.hermes/skills/<category>/<name>` on every
  # start, and it now ships its own `google-workspace` skill under
  # `productivity/`. Two skills with the same frontmatter `name:` make
  # `skill_view` refuse to guess ("Ambiguous skill name"), which silently
  # broke the cron jobs that preload the skill by name.
  #
  # Registering the directory under `skills.external_dirs` (see
  # assets/config.yaml) makes hermes defer to our copy: it skips
  # writing the bundled one into the local tree and removes any stale
  # shadow it had already written, so the name resolves unambiguously.
  home.file = {
    ".hermes/plugins/current-datetime" = {
      source = hermesCurrentDatetimePlugin;
      recursive = true;
    };

    ".hermes/skills-external/nixomatic" = {
      source = hermesNixomaticSkill;
      recursive = true;
    };

    ".hermes/skills-external/google-workspace" = {
      source = hermesGoogleWorkspaceSkill;
      recursive = true;
    };
  };
}
