{ mkDerivation, array, base, containers, HUnit, QuickCheck, stdenv
, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "course";
  version = "0.1.4";
  src = ./.;
  libraryHaskellDepends = [ array base containers ];
  testHaskellDepends = [
    base HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/data61/fp-course";
  description = "Source code for a functional programming course";
  license = stdenv.lib.licenses.bsd3;
}
