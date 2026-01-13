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
      browse-kill-ring
      company
      dracula-theme
      dumb-jump
      git-link
      gptel
      helm
      helm-company
      helm-lsp
      helm-project
      lsp-mode
      magit
      neotree
      nix-mode
      org-journal
      powerline
      rainbow-delimiters
      sublimity
      treesit-auto
      undo-tree
      use-package
      yasnippet
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
        exec env XDG_RUNTIME_DIR="$HOME/.emacs.d" ${emacs}/bin/emacsclient "$@"
      '')
    else emacs;

  emacsBinary =
    if mainlyRemote || nox
    then "${maybeWrappedEmacsClient customEmacs}/bin/emacsclient --tty"
    else
      (let
        script = pkgs.writeShellScriptBin "emacsclient" ''
          exec ${maybeWrappedEmacsClient customEmacs}/bin/emacsclient --create-frame --no-wait -e "(progn (select-frame-set-input-focus (selected-frame)) (toggle-frame-maximized) (find-file (expand-file-name \"$1\")))" &> /dev/null
        '';
      in "${script}/bin/emacsclient");
in {
  inherit customEmacs emacsBinary treesit-grammars;
}
