{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let package = "cpspg";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope =
          on.buildDuneProject { resolveArgs.with-test = true; } package ./. { ocaml-base-compiler = "*"; };
        overlay = final: prev:
          {
            # Your overrides go here
            cpspg = prev.cpspg.override {
              doCheck = true;
              checkPhase = ''
                dune runtest
              '';
            };

          };
      in rec {
        legacyPackages = scope.overrideScope overlay;

        packages = rec {
          default = self.legacyPackages.${system}.${package};
          cpspg = default;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = packages.default.buildInputs;
        };
      });
}
