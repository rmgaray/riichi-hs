{
  description = "Packaging, devShells and hooks for riichi";

  inputs = {
    # We temporarily use the "haskell-updates" branch of Nixpkgs to get
    # HLS 2.11, which supports code actions for the eval plugin.
    nixpkgs-hs-updates.url = "github:NixOS/nixpkgs/haskell-updates";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
    typedKanrenSrc = {
      url = "github:snejugal/typedKanren/d6d998c80624f6b71d853565e650fecf76be0203";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs-hs-updates,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
    typedKanrenSrc,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "ghc967";
      haskell-lsp = nixpkgs-hs-updates.legacyPackages.${system}.pkgs.haskell.packages.${ghcVersion}.haskell-language-server;
      pkgs = nixpkgs.legacyPackages.${system}.pkgs;
      hsPkgs = pkgs.haskell.packages.${ghcVersion};
      # Haskell packages extended with typedKanren
      hsPkgs' = hsPkgs.extend (final: _prev: {
        typedKanren = final.callPackage (final.callCabal2nix "typedKanren" typedKanrenSrc) {};
      });
      compiler = pkgs.haskell.compiler.${ghcVersion};
    in {
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            ormolu.enable = true;
            cabal2nix.enable = true;
            alejandra.enable = true;
          };
        };
      };

      devShell = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        nativeBuildInputs = with pkgs; [
          haskell-lsp
          cabal-install
          cabal2nix
          happy
          compiler
        ];
      };

      packages.riichi = hsPkgs'.callPackage ./default.nix {};
    });
}
