{
  mainlyRemote,
  nox,
  isDarwin ? false,
}: {pkgs}: let
  maybeWrappedEmacsClient = emacs:
    if isDarwin
    then
      (pkgs.writeShellScriptBin "emacsclient" ''
        exec env XDG_RUNTIME_DIR="$HOME/.emacs.d" ${emacs}/bin/emacsclient "$@"
      '')
    else emacs;
  emacsBinary =
    if mainlyRemote || nox
    then "${maybeWrappedEmacsClient pkgs.emacs-nox}/bin/emacsclient --tty"
    else
      (let
        script = pkgs.writeShellScriptBin "emacsclient" ''
          exec ${maybeWrappedEmacsClient pkgs.emacs}/bin/emacsclient --create-frame --no-wait -e "(progn (select-frame-set-input-focus (selected-frame)) (toggle-frame-maximized) (find-file (expand-file-name \"$1\")))" &> /dev/null
        '';
      in "${script}/bin/emacsclient");
in
  emacsBinary
