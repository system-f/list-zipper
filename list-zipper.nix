{ mkDerivation, base, checkers, comonad, deriving-compat, lens, mtl
, QuickCheck, semigroupoids, semigroups, stdenv, tasty, tasty-hunit
, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "list-zipper";
  version = "0.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base comonad deriving-compat lens mtl semigroupoids semigroups
    transformers
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/list-zipper";
  description = "A list zipper";
  license = stdenv.lib.licenses.bsd3;
}
