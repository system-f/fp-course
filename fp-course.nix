{ mkDerivation, array, base, Cabal, cabal-doctest, containers
, doctest, HUnit, QuickCheck, stdenv, tasty, tasty-discover
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "course";
  version = "0.1.4";
  src = ./.;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    array base containers doctest QuickCheck
  ];
  testHaskellDepends = [
    base HUnit QuickCheck tasty tasty-discover tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/data61/fp-course";
  description = "Source code for a functional programming course";
  license = stdenv.lib.licenses.bsd3;
}
