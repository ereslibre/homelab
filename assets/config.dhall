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
            pkgs = nixities.packages.''${system};
            nixpkgs = nixities.nixpkgs.legacyPackages.''${system};
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
