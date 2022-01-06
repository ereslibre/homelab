.PHONY: fmt install-nix switch

switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".activationPackage'

install-nix:
	curl -L https://nixos.org/nix/install | sh

fmt:
	find . -name "*.nix" | xargs nixfmt

lint:
	find . -name "*.nix" | xargs nix-linter
