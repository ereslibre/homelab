\(args : { derivations : Text }) ->
  ''
  {
    inputs = {
      nixities.url = "github:ereslibre/nixities";
      flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixities, flake-utils }:
      flake-utils.lib.eachDefaultSystem
        (system:
          let
            pkgs = (import nixities.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            });
            pkgs-cuda = (import nixities.nixpkgs {
              inherit system;
              config = {
                allowUnfree = true;
                cudaSupport = true;
              };
            });
          in {
            devShells.default = nixities.nixpkgs.legacyPackages.''${system}.mkShell {
              buildInputs = [
                ${args.derivations}
              ];
            };
          }
        );
  }
  ''
