# dotfiles

My dotfiles. Set as a [home-manager](https://github.com/nix-community/home-manager)
[flake](https://nixos.wiki/wiki/Flakes).

## Configure

1. Install the [`nix`](https://nixos.org/) package manager.

   ```console
   $ make install-nix
   ```

1. Activate the home-manager profile matching the machine you are at.

   ```console
   $ make
   ```

> Note: on Mac OS X, run afterwards:
>
> ```bash
> $ launchctl load ~/Library/LaunchAgents/es.ereslibre.emacs.plist
> ```
>
> So the agent is loaded without the need to restart the session,
> starting the emacs daemon.


Done! :)
