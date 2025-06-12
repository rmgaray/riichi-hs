{
  mkDerivation,
  base,
  lens,
  lib,
  typedKanren,
}:
mkDerivation {
  pname = "riichi-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [base lens typedKanren];
  executableHaskellDepends = [base];
  description = "A set of tools for Riichi Mahjong players";
  license = lib.licenses.gpl3Only;
  mainProgram = "riichi-hs";
}
