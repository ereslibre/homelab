# llama-gui in front of llama.cpp, serving what ../llm.nix describes.
{
  services.llama-gui = let
    llm = import ../llm.nix;
  in {
    enable = true;
    host = "0.0.0.0";
    # The model below at its full context, and nothing else beside it.
    settings.LLAMA_GUI_MODELS_MAX = 1;
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
  };

  systemd.services.llama-gui = {
    after = ["nvidia-persistenced.service"];
    requires = ["nvidia-persistenced.service"];
  };
}
