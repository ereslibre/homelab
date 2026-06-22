{profile}: {
  home.file = {
    ".hushlogin".text = "";
    ".emacs.d/custom.el" = {
      source = ./assets/emacs/emacs.d/custom.el;
    };
    ".pi/agent/models.json".text = builtins.toJSON {
      providers = {
        ollama = {
          baseUrl = "http://hulk.ereslibre.net:11434/v1";
          api = "openai-completions";
          apiKey = "ollama";
          # hulk serves the unsloth MTP GGUF (global LLAMA_ARG_SPEC_TYPE=draft-mtp
          # requires an MTP/nextn head); gemma4:31b was dropped there.
          models = [
            {id = "hf.co/unsloth/Qwen3.6-27B-MTP-GGUF:Q5_K_M";}
          ];
        };
      };
    };
  };
}
