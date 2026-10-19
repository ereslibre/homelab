{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.claude-sessions;
  package = pkgs.callPackage ./claude-sessions/package.nix {};

  # Every event the state file follows; see claude-sessions/src/hook.rs.
  events = [
    "SessionStart"
    "UserPromptSubmit"
    "PreToolUse"
    "PostToolUse"
    "Notification"
    "Stop"
    "SessionEnd"
  ];
  hooks = lib.genAttrs events (_: [
    {
      hooks = [
        {
          type = "command";
          command = "${lib.getExe package} hook";
          timeout = 5;
        }
      ];
    }
  ]);

  # A plugin rather than `programs.claude-code.settings.hooks`: setting
  # anything there makes settings.json a read-only store link, and
  # agents.nix keeps it mutable on purpose.
  json = pkgs.formats.json {};
  plugin = pkgs.runCommand "claude-sessions-plugin" {} ''
    install -Dm644 ${json.generate "plugin.json" {
      name = "claude-sessions";
      inherit (package) version;
      description = "Record each Claude Code session's state for claude-sessions";
    }} $out/.claude-plugin/plugin.json
    install -Dm644 ${json.generate "hooks.json" {inherit hooks;}} $out/hooks/hooks.json
  '';
in {
  options.services.claude-sessions = {
    enable = lib.mkEnableOption ''
      the claude-sessions MCP server, which lists, reads and types into the
      Claude Code sessions running in tmux. Anyone who can reach it can run
      anything those sessions can, so keep it behind a firewall'';
    listen = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1:8765";
      example = "0.0.0.0:8765";
      description = "Address the server listens on.";
    };
    allowedHosts = lib.mkOption {
      type = with lib.types; listOf str;
      default = [];
      example = ["hulk" "hulk.example.ts.net"];
      description = ''
        `Host` headers the server accepts, with or without port. Empty means
        loopback only, so a server reached from another machine needs the
        names that machine uses for this one.
      '';
    };
  };

  config = lib.mkMerge [
    # The hooks go wherever Claude Code does: they are cheap, and the state
    # is there for the server to pick up once it is enabled.
    (lib.mkIf config.programs.claude-code.enable {
      programs.claude-code.plugins.claude-sessions = plugin;
    })
    (lib.mkIf cfg.enable {
      # A user unit, so it reaches the user's default tmux server and the
      # state files the hooks write. Without lingering it stops at logout.
      systemd.user.services.claude-sessions = {
        Unit.Description = "MCP server for the Claude Code sessions in tmux";
        Service = {
          ExecStart = lib.escapeShellArgs (
            [(lib.getExe package) "serve" "--listen" cfg.listen]
            ++ lib.concatMap (host: ["--allowed-host" host]) cfg.allowedHosts
          );
          Restart = "on-failure";
        };
        Install.WantedBy = ["default.target"];
      };
    })
  ];
}
