# What hulk's llama-gui serves, for the clients that have to agree with it:
# the pi agent in ./dotfiles/agents.nix here, and holabot on nuc-3 in the
# private repo, which reads it as `homelab.lib.llm`.
#
# llama-gui keeps its launch profiles in its own database, not in Nix, so
# these values are what the profile on hulk has to be created with (see
# services.llama-gui in ./hulk/configuration.nix).
{
  # OpenAI-compatible endpoint, llama-gui's proxy in front of llama-server.
  baseUrl = "http://hulk.ereslibre.net:11434/v1";
  # The profile's alias, which is the `model` clients ask for.
  model = "qwen3.8-27b";
  # The profile's context size. Clients that assume more get their prompts
  # rejected or truncated by the server.
  contextLength = 131072;
}
