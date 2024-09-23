{
  description = "Simple flake setup for doing Advent of Code in OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlpkgs = pkgs.ocamlPackages;
        sources = {
          ocaml = nix-filter.lib {
            root = ./.;
            include = [
              ".ocamlformat"
              "dune-project"
              "dune"
              (nix-filter.lib.matchExt "ml")
            ];
          };
          nix = nix-filter.lib {
            root = ./.;
            include = [ (nix-filter.lib.matchExt "nix") ];
          };
        };
      in {
        packages = {
          default = self.packages.${system}.aoc;
          aoc = ocamlpkgs.buildDunePackage {
            pname = "aoc";
            version = "0.1.0";
            duneVersion = "3";
            src = sources.ocaml;

            buildInputs = [
              ocamlpkgs.cohttp
              ocamlpkgs.cohttp-lwt
              ocamlpkgs.re
              ocamlpkgs.angstrom
              ocamlpkgs.ppx_deriving
            ];

            strictDeps = true;

            preBuild = ''
              dune build aoc.opam
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.nixpkgs-fmt
            pkgs.ocamlformat
            pkgs.fswatch
            ocamlpkgs.odoc
            ocamlpkgs.ocaml-lsp
            ocamlpkgs.ocamlformat-rpc-lib
            ocamlpkgs.utop
          ];

          inputsFrom = [ self.packages.${system}.aoc ];
        };
      });
}
