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
          # Keep in sync with services.ollama.loadModels on hulk.
          models = [
            {id = "hf.co/unsloth/Qwen3.8-27B-GGUF:UD-Q4_K_XL";}
          ];
        };
      };
    };
  };
}
