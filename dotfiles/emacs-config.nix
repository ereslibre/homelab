{
  mainlyRemote,
  nox,
  isDarwin ? false,
}: {pkgs}: let
  inherit (pkgs) lib;

  # Every non-broken grammar derivation exposed by nixpkgs' tree-sitter overlay.
  # We build the link farm ourselves rather than via `pkgs.tree-sitter.withPlugins`
  # to sidestep an upstream bug in its `mkGrammarLinkFarm` (replaceStrings passed a
  # string instead of a list); this keeps working once nixpkgs reverts the regression.
  grammarDrvs = pkgs.tree-sitter.allGrammars;

  # Collect every libtree-sitter-*.so so Emacs can load any mode automatically.
  # Each grammar derivation ships its compiled parser at `${drv}/parser`; name it
  # libtree-sitter-<lang>.so, mapping hyphens to underscores (e.g. c-sharp -> c_sharp).
  treesit-grammars = pkgs.runCommand "treesit-grammars" {} (''
      mkdir -p $out/lib
    ''
    + lib.concatMapStringsSep "\n" (drv: let
      lang = lib.replaceStrings ["-"] ["_"] (
        lib.removePrefix "tree-sitter-" (lib.removeSuffix "-grammar" (lib.getName drv))
      );
    in ''
      ln -sf ${drv}/parser "$out/lib/libtree-sitter-${lang}.so"
    '')
    grammarDrvs);

  # Emacs packages from your init.el
  emacsPackages = epkgs:
    with epkgs; [
      ace-window
      adoc-mode
      browse-kill-ring
      carbon-now-sh
      company
      dockerfile-mode
      doom-themes
      dracula-theme
      dumb-jump
      envrc
      git-link
      google-translate
      helm
      helm-company
      helm-lsp
      helm-project
      lsp-mode
      lsp-ui
      magit
      markdown-mode
      monokai-theme
      org-journal
      perfect-margin
      powerline
      rainbow-delimiters
      rg
      treesit-fold
      undo-tree
      use-package
      writeroom-mode
      yasnippet

      # Language modes
      ada-ts-mode
      agda2-mode
      auctex
      crystal-mode
      cue-mode
      d-mode
      dhall-mode
      elm-mode
      erlang
      fennel-mode
      fish-mode
      fsharp-mode
      go-mode
      graphql-mode
      groovy-mode
      haskell-mode
      hcl-mode
      jq-mode
      json-mode
      jsonnet-mode
      just-mode
      kdl-mode
      ledger-mode
      matlab-mode
      mermaid-mode
      meson-mode
      nim-mode
      nix-mode
      powershell
      prisma-mode
      prolog-mode
      purescript-mode
      rego-mode
      rescript-mode
      rust-mode
      scss-mode
      slint-mode
      svelte-mode
      templ-ts-mode
      terraform-mode
      thrift
      # nixpkgs' melpa snapshot still fetches ttl-mode from
      # github.com/emacsattic/ttl-mode, and that repo is gone -- the
      # fixed-output fetch 404s and takes the whole emacs closure down
      # with it. MELPA upstream has already re-pointed its recipe at
      # jeeger/ttl-mode; this only gets there ahead of the next nixpkgs
      # regeneration, so drop the override once
      # `emacsPackages.ttl-mode.src` names jeeger on its own.
      (ttl-mode.overrideAttrs (_: {
        src = pkgs.fetchFromGitHub {
          owner = "jeeger";
          repo = "ttl-mode";
          rev = "5f7604e4c88c8d5a8c899a4b4aa95c8fc0bfb09c";
          hash = "sha256-Nyb+m6yRFB7UC2Vp5cJIFYnIoG3nc58iJWFenTQXFog=";
        };
      }))
      tuareg
      typst-ts-mode
      v-mode
      vala-mode
      vimrc-mode
      wgsl-mode
      yaml-mode
      yang-mode
      yara-mode
      zig-mode
    ];

  # Emacs with packages and tree-sitter support
  customEmacs =
    if mainlyRemote || nox
    then (pkgs.emacsPackagesFor pkgs.emacs31-nox).emacsWithPackages emacsPackages
    else (pkgs.emacsPackagesFor pkgs.emacs31).emacsWithPackages emacsPackages;

  # Wrapper for emacsclient
  maybeWrappedEmacsClient = emacs:
    if isDarwin
    then
      (pkgs.writeShellScriptBin "emacsclient" ''
        exec env XDG_RUNTIME_DIR="$HOME/.emacs.d" ${lib.getExe' emacs "emacsclient"} "$@"
      '')
    else emacs;

  emacsBinary =
    if mainlyRemote || nox
    then "${lib.getExe' (maybeWrappedEmacsClient customEmacs) "emacsclient"} --tty"
    else
      (let
        script = pkgs.writeShellScriptBin "emacsclient" ''
          exec ${lib.getExe' (maybeWrappedEmacsClient customEmacs) "emacsclient"} --create-frame --no-wait -e "(progn (select-frame-set-input-focus (selected-frame)) (toggle-frame-maximized) (find-file (expand-file-name \"$1\")))" &> /dev/null
        '';
      in "${lib.getExe script}");
in {
  inherit customEmacs emacsBinary treesit-grammars;
}
