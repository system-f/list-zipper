{ mkDerivation, base, checkers, comonad, deriving-compat, lens
, QuickCheck, semigroupoids, semigroups, stdenv, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "list-zipper";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base comonad deriving-compat lens semigroupoids semigroups
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/list-zipper";
  description = "A list zipper";
  license = stdenv.lib.licenses.bsd3;
}
