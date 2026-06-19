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
          models = [
            {id = "qwen3-coder:30b";}
            {id = "qwen3.6:27b";}
            {id = "gemma4:latest";}
          ];
        };
      };
    };
  };
}
