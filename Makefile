.PHONY: switch
switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".config.activationPackage'

.PHONY: install-nix
install-nix:
	curl -L https://nixos.org/nix/install | sh

.PHONY: fmt
fmt:
	find . -name "*.nix" | xargs nix develop --command alejandra

.PHONY: update-emacs-d
update-emacs-d:
	scripts/update-emacs-d.sh
