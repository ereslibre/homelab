{profile}: {
  home.file = {
    ".hushlogin".text = "";
    # revdiff reads this INI file directly (~/.config/revdiff/config), so
    # the settings apply to plugin-spawned overlays too, where env vars do
    # not survive the multiplexer popup's fresh shell.
    ".config/revdiff/config".text = ''
      theme = dracula
      emacs = true
    '';
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
