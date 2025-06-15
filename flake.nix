{
  description = "Packaging, devShells and hooks for riichi";

  # Reference for static builds: https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix

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
      inherit (nixpkgs.legacyPackages.${system}.pkgs) lib;
      ### Compiler
      ghcVersion = "ghc967";
      compiler = pkgs.haskell.compiler.${ghcVersion};
      ### Overlays
      # Adds typedKanren to `haskell.packages`
      typedKanrenOverlay = final: prev: {
        typedKanren = final.callPackage (final.callCabal2nix "typedKanren" typedKanrenSrc) {};
      };
      # Fixes issues with TH in static builds
      staticCompilerOverlay = final: prev: {
        ghc =
          prev.ghc.override
          {
            enableRelocatedStaticLibs = true;
            enableShared = false;
            enableDwarf = false;
          };
      };
      # Apply overlays
      # TODO: factor out common logic
      pkgs =
        nixpkgs.legacyPackages.${system}.pkgs.extend
        (
          _f: prev: {
            haskell =
              prev.haskell
              // {
                packages =
                  prev.haskell.packages
                  // {
                    "${ghcVersion}" = prev.haskell.packages."${ghcVersion}".extend typedKanrenOverlay;
                  };
              };
          }
        );
      pkgsMusl =
        nixpkgs.legacyPackages.${system}.pkgsMusl.extend
        (
          f: prev: {
            haskell =
              prev.haskell
              // {
                packages =
                  prev.haskell.packages
                  // {
                    "${ghcVersion}" =
                      (prev.haskell.packages."${ghcVersion}".extend staticCompilerOverlay).extend typedKanrenOverlay;
                  };
                # compiler = prev.haskell.compiler //
                #   {
                #     "${ghcVersion}" = prev.haskell.compiler."${ghcVersion}".override
                #     {
                #       enableRelocatedStaticLibs = true;
                #       enableShared = false;
                #       enableDwarf = false;
                #     };
                #   };
              };
          }
        );
      hsPkgs = pkgs.haskell.packages.${ghcVersion};
      hsPkgsMusl = pkgsMusl.haskell.packages.${ghcVersion};
      utils = import ./utils.nix pkgsMusl;
      # TODO: Drop haskell-updates input when HLS 2.11 lands in staging
      haskell-lsp = nixpkgs-hs-updates.legacyPackages.${system}.pkgs.haskell.packages.${ghcVersion}.haskell-language-server;
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

      packages.riichi = hsPkgs.callPackage ./default.nix {};
      packages.riichi_static =
        utils.overrideMakeStatic
        (hsPkgsMusl.callPackage ./default.nix {});
    });
}
