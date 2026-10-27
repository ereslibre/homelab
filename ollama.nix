# What hulk's Ollama serves, for the clients that have to agree with it:
# the pi agent in ./dotfiles/agents.nix here, and holabot on nuc-3 in the
# private repo, which reads it as `homelab.lib.ollama`.
{
  # OLLAMA_CONTEXT_LENGTH on hulk. Clients that assume more get their
  # prompts silently truncated by the server.
  contextLength = 131072;
}
