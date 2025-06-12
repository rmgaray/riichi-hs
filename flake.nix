{
  description = "Devshells and pre-commit hooks for riichi-hs";

  inputs = {
    # We temporarily use the "haskell-updates" branch of Nixpkgs to get
    # HLS 2.11, which supports code actions for the eval plugin.
    nixpkgs-hs-updates.url = "github:NixOS/nixpkgs/haskell-updates";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs-hs-updates,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "ghc967";
      pkgs = nixpkgs.legacyPackages.${system};
      haskell-lsp = nixpkgs-hs-updates.legacyPackages.${system}.pkgs.haskell.packages.${ghcVersion}.haskell-language-server;
      hsPkgs = pkgs.haskell.packages.${ghcVersion};
      hsCompiler = pkgs.haskell.compiler.${ghcVersion};
    in {
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            ormolu.enable = true;
            alejandra.enable = true;
          };
        };
      };

      devShell = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        nativeBuildInputs = with hsPkgs; [
          haskell-lsp
          cabal-install
          happy
          hsCompiler
        ];
      };
    });
}
