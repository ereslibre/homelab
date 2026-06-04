{
  mainlyRemote,
  nox,
  isDarwin ? false,
}: {pkgs}: let
  inherit (pkgs) lib;

  # Build all grammars exposed by nixpkgs' tree-sitter overlay
  treeSitterBundle = pkgs.tree-sitter.withPlugins (available:
    builtins.attrValues (
      lib.filterAttrs
      (_: drv: lib.isDerivation drv && !(drv.meta.broken or false))
      available
    ));

  # Collect every libtree-sitter-*.so so Emacs can load any mode automatically
  treesit-grammars = pkgs.runCommand "treesit-grammars" {} ''
    mkdir -p $out/lib
    find -L "${treeSitterBundle}" -type f | while IFS= read -r so; do
      case "$so" in
        *.so | *.dylib) ;;
        *) continue ;;
      esac
      name="$(basename "$so")"
      ext="''${name##*.}"
      if [ "''${name#libtree-sitter-}" = "$name" ]; then
        lang="''${name%.*}"
        name="libtree-sitter-$lang.$ext"
      fi
      ln -sf "$so" "$out/lib/$name"
    done
  '';

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
      gptel
      helm
      helm-company
      helm-lsp
      helm-project
      lsp-mode
      lsp-ui
      magit
      markdown-mode
      monokai-theme
      neotree
      org-journal
      powerline
      rainbow-delimiters
      rg
      sublimity
      treesit-auto
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
      ttl-mode
      tuareg
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
    then (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages emacsPackages
    else (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages emacsPackages;

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
