# llama-gui in front of llama.cpp, serving what ../llm.nix describes.
{
  services.llama-gui = let
    llm = import ../llm.nix;
  in {
    enable = true;
    host = "0.0.0.0";
    # The same port Ollama uses.
    port = 11434;
    # One slot per profile below, so loading one never evicts the other.
    settings = {
      LLAMA_GUI_MODELS_MAX = 2;
      # Every route (UI, API and the proxy clients use) rejects a Host that
      # looks public unless it is listed here, and this one is how ../llm.nix
      # and the browser reach hulk. Single-label names and IPs always pass.
      LLAMA_GUI_ALLOWED_HOSTS = "hulk.ereslibre.net";
    };
    profiles.${llm.model} = {
      model = "hf.co/unsloth/Qwen3.8-27B-GGUF:UD-Q4_K_XL";
      settings = {
        name = "Qwen3.8 27B";
        ctx_size = llm.contextLength;
        parallel = 1;
        ngl = "all";
        flash_attn = "on";
        cache_type_k = "q8_0";
        cache_type_v = "q8_0";
        load_at_start = true;
        # qwen3.5 is hybrid SSM/attention, so llama.cpp snapshots the recurrent
        # state instead of rewinding it. Each checkpoint is ~150MiB here and
        # the default cap is 32 per slot -- 4.7GiB that accrues over a long
        # session, after the fit has already been sized.
        extra_args = ["--ctx-checkpoints 8"];
      };
    };
    # A decision model (llama.cpp's /v1/systemone), 430MiB of ModernBERT.
    profiles.${llm.decisionModel} = {
      model = "hf.co/ggml-org/Laya-GGUF:Q8_0";
      settings = {
        name = "Laya";
        # The model's own limit.
        ctx_size = 8192;
        # llama.cpp evaluates a Laya prompt in one batch and rejects one that
        # does not fit, so the batch is as large as the context.
        batch = 8192;
        ubatch = 8192;
        parallel = 1;
        # On the CPU: the GPU is sized for the model above, and this one must
        # never be what keeps it from loading.
        ngl = "0";
        load_at_start = true;
      };
    };
  };

  systemd.services.llama-gui = {
    after = ["nvidia-persistenced.service"];
    requires = ["nvidia-persistenced.service"];
  };
}
