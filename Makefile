.PHONY: fmt install-nix switch

switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".activationPackage'

switch-on-user-at-host:
	nix run '.#homeConfigurations.${ACTIVATION_USER}@${ACTIVATION_HOST}.activationPackage'

install-nix:
	curl -L https://nixos.org/nix/install | sh

fmt:
	find . -name "*.nix" | xargs nix shell nixpkgs#nixfmt --command nixfmt -c

lint:
	nix shell nixpkgs#nix-linter --command nix-linter -r
