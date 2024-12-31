{lib, ...}: {
  home.sessionVariables = {
    TERM = lib.mkForce "xterm-256color";
  };
}
