.PHONY: fmt switch

switch:
	nix run '.#homeConfigurations."${USER}@$(shell hostname -s)".activationPackage'

fmt:
	find . -name "*.nix" | xargs nixfmt
