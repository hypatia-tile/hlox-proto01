{
  description = "Haskell environment for hlox";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};

        hp = pkgs.haskell.packages.ghc910;

        # Pick a compiler once.
        commonTools = [
          hp.ghc
          hp.cabal-install
          hp.hoogle
          hp.haskell-language-server
          hp.ghcid
          hp.ormolu
          hp.hlint
        ];
      in {
        devShells.default = pkgs.mkShell {
          name = "hlox-dev-shell";
          packages = commonTools;
          shellHook = ''
            echo "Haskell research shell:"
            echo "ghc=$(ghc --numeric-version),"
            echo "cabal=$(cabal --numeirc-verision),"
            echo "Try: cabal repl | ghci | ghcid --command 'cabal repl'"
          '';
        };
      }
    );
}
