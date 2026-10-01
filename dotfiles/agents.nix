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
        package = agents.claude-code;
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
      github-copilot-cli = {
        enable = true;
        package = agents.copilot-cli;
      };
      opencode = {
        enable = true;
        package = agents.opencode;
      };
      pi-coding-agent = {
        enable = true;
        package = agents.pi;
        models.providers.ollama = {
          baseUrl = "http://hulk.ereslibre.net:11434/v1";
          api = "openai-completions";
          apiKey = "ollama";
          # Keep in sync with services.ollama.loadModels on hulk.
          models = [
            {id = "hf.co/unsloth/Qwen3.8-27B-GGUF:UD-Q4_K_XL";}
          ];
        };
      };
    };
  }
