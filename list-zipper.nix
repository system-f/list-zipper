{ mkDerivation, base, checkers, comonad, deriving-compat, hedgehog
, hedgehog-fn, lens, mtl, QuickCheck, semigroupoids, semigroups
, stdenv, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "list-zipper";
  version = "0.0.5";
  src = ./.;
  libraryHaskellDepends = [
    base comonad deriving-compat lens mtl semigroupoids semigroups
    transformers
  ];
  testHaskellDepends = [
    base checkers hedgehog hedgehog-fn lens QuickCheck tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/list-zipper";
  description = "A list zipper";
  license = stdenv.lib.licenses.bsd3;
}
