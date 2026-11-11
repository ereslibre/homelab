{aiTools}: {
  llm-agents,
  pkgs,
  lib,
  ...
}: let
  # Skills shared by every agent. Each is a directory holding a SKILL.md
  # (`name` + `description` frontmatter), which Claude Code runs as
  # `/<name>` and Codex as `$<name>`.
  skills = ["address-review"];
  skillFiles = dir:
    lib.genAttrs' skills (name:
      lib.nameValuePair "${dir}/${name}/SKILL.md" {source = ./assets/agents/${name}.md;});
  agents = llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
in
  lib.mkIf aiTools {
    home.file = skillFiles ".claude/skills" // skillFiles ".codex/skills";

    # Agents with a Home Manager module, fed the llm-agents build instead of the
    # nixpkgs one; the rest stay plain packages in packages.nix. These modules
    # only write config files for options that are set, so the agents' own
    # settings files stay mutable.
    programs = {
      claude-code = {
        enable = true;
        # Claude Code shells out to python3 for edits and data munging, and
        # falls back to slower paths when it is missing. Suffixed so a
        # project-local python (devenv/.envrc) still wins. `version` is kept
        # so the module's version-gated features keep working.
        package = pkgs.symlinkJoin {
          inherit (agents.claude-code) pname version meta;
          name = "claude-code-${agents.claude-code.version}";
          paths = [agents.claude-code];
          nativeBuildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/claude --suffix PATH : ${lib.makeBinPath [pkgs.python3]}
          '';
        };
        # Same servers as the official `rust-analyzer-lsp` and `gopls-lsp`
        # marketplace plugins, shipped as a Home Manager-managed plugin instead.
        # Commands resolve through PATH on purpose: the global binaries come
        # from packages.nix, and project-local toolchains (devenv/.envrc)
        # override them.
        lspServers = {
          gopls = {
            command = "gopls";
            extensionToLanguage.".go" = "go";
          };
          rust-analyzer = {
            command = "rust-analyzer";
            extensionToLanguage.".rs" = "rust";
          };
        };
      };
      codex = {
        enable = true;
        package = agents.codex;
      };
      opencode = {
        enable = true;
        package = agents.opencode;
      };
      pi-coding-agent = {
        enable = true;
        package = agents.pi;
        models.providers.hulk = {
          inherit (import ../llm.nix) baseUrl;
          api = "openai-completions";
          # llama-gui has no authentication yet; pi just needs a value.
          apiKey = "none";
          models = [
            {
              id = (import ../llm.nix).model;
              # Without `reasoning` pi sends no thinking control at all and
              # the model defaults to thinking on. Map pi's "off" to
              # `reasoning_effort` "none". The chat template rejects anything
              # above "high" with a 500.
              reasoning = true;
              thinkingLevelMap = {
                off = "none";
                xhigh = "high";
                max = "high";
              };
              compat.supportsReasoningEffort = true;
              contextWindow = (import ../llm.nix).contextLength;
            }
          ];
        };
      };
    };
  }
