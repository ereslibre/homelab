.PHONY: switch
switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".activationPackage'

.PHONY: activate-user-at-host
activate-user-at-host:
	nix run '.#homeConfigurations.${ACTIVATION_USER}@${ACTIVATION_HOST}.activationPackage'

.PHONY: install-nix
install-nix:
	curl -L https://nixos.org/nix/install | sh

.PHONY: fmt
fmt:
	find . -name "*.nix" | xargs nix develop --command nixfmt

.PHONY: lint
lint:
	nix develop --command nix-linter -r

.PHONY: update-emacs-d
update-emacs-d:
	scripts/update-emacs-d.sh
