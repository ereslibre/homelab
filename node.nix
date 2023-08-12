{
  home-manager,
  pkgs,
  ...
}: {
  # Enabling linger makes the systemd user services start
  # automatically. In this machine, I want to trigger the
  # `gpg-forward-agent-path` service file automatically as
  # systemd starts, so the socket dir is always created and I
  # can forward the GPG agent through SSH directly without
  # having a first failed connection due to a missing
  # `/run/user/<id>/gnupg`.
  home.activation.linger = home-manager.lib.hm.dag.entryBefore ["reloadSystemd"] ''
    ${pkgs.systemd}/bin/loginctl enable-linger $USER
  '';

  programs = {
    bash.shellAliases = {
      gpg = "${pkgs.gnupg}/bin/gpg --no-autostart";
    };
    keychain = {
      enable = true;
      enableZshIntegration = true;
      keys = [];
      inheritType = "any";
    };
    zsh.shellAliases = {
      gpg = "${pkgs.gnupg}/bin/gpg --no-autostart";
    };
  };
}
