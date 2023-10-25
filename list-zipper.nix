{ mkDerivation, base, checkers, comonad, deriving-compat, hedgehog
, hedgehog-fn, lens, lib, MonadRandom, mtl, QuickCheck
, random-shuffle, semigroupoids, semigroups, tasty, tasty-hedgehog
, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "list-zipper";
  version = "0.0.11";
  src = ./.;
  libraryHaskellDepends = [
    base comonad deriving-compat lens MonadRandom mtl random-shuffle
    semigroupoids semigroups transformers
  ];
  testHaskellDepends = [
    base checkers hedgehog hedgehog-fn lens mtl QuickCheck tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/system-f/list-zipper";
  description = "A list zipper";
  license = lib.licenses.bsd3;
}
