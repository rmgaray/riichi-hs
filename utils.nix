{pkgs, ...}: {
  # Instruct GHC to compile libraries statically
  overrideMakeStatic = pkg:
    pkgs.haskell.lib.overrideCabal pkg (old: {
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags =
        (old.configureFlags or [])
        ++ [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override {withStatic = true;}}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: {dontDisableStatic = true;})}/lib"
        ];
    });
}
