ACTIVATION_USER ?= ${USER}
ACTIVATION_HOST ?= $(shell hostname -s)

.PHONY: switch
switch:
	nix run '.#homeConfigurations."${ACTIVATION_USER}@${ACTIVATION_HOST}".config.activationPackage'

.PHONY: build
build:
	nix build '.#homeConfigurations."${ACTIVATION_USER}@${ACTIVATION_HOST}".config.activationPackage'

.PHONY: install-nix
install-nix:
	curl -L https://nixos.org/nix/install | sh

.PHONY: fmt
fmt:
	find . -name "*.nix" | xargs nix develop --command alejandra

.PHONY: update-emacs-d
update-emacs-d:
	scripts/update-emacs-d.sh
