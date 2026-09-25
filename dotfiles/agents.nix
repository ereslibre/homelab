{aiTools}: {lib, ...}: let
  # Skills shared by every agent. Each is a directory holding a SKILL.md
  # (`name` + `description` frontmatter), which Claude Code runs as
  # `/<name>` and Codex as `$<name>`.
  skills = ["address-review"];
  skillFiles = dir:
    lib.genAttrs' skills (name:
      lib.nameValuePair "${dir}/${name}/SKILL.md" {source = ./assets/agents/${name}.md;});
in {
  home.file = lib.mkIf aiTools (skillFiles ".claude/skills" // skillFiles ".codex/skills");
}
