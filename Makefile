.PHONY: activate-user-at-host fmt install-nix lint switch

switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".activationPackage'

activate-user-at-host:
	nix run '.#homeConfigurations.${ACTIVATION_USER}@${ACTIVATION_HOST}.activationPackage'

install-nix:
	curl -L https://nixos.org/nix/install | sh

fmt:
	find . -name "*.nix" | xargs nix develop --command nixfmt

lint:
	nix develop --command nix-linter -r
