{ mkDerivation, base, bytestring, hspec, lib, process, QuickCheck
, time, unix
}:
mkDerivation {
  pname = "hcat";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring process time unix ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base bytestring hspec QuickCheck ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
