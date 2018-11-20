{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative(pure, (*>))
import Data.Bool(Bool)
import Data.Eq(Eq)
import Data.Foldable(traverse_)
import Data.Function(($))
import Data.ListZipper(ListZipper(ListZipper), ListZipperOp', moveLeft, moveRight, moveLeftUntil, moveRightUntil, moveLeftRightUntil, moveRightLeftUntil, moveLeftUntilThen, moveRightUntilThen, moveLeftRightUntilThen, moveRightLeftUntilThen, list, (##>), deleteStepLeft, deleteStepRight, runListZipperOp)
import Data.String(String)
import Hedgehog(Gen, Property, property, forAll, forAllWith, (===))
import Hedgehog.Function(Arg, Vary, forAllFn, fn)
import qualified Hedgehog.Gen as Gen(list, element, bool, int)
import qualified Hedgehog.Range as Range(linear)
import Prelude(Show)
import System.IO(IO)
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog(testProperty)

main ::
  IO ()
main =
  defaultMain listzipper_properties

genListZipper ::
  Gen a
  -> Gen (ListZipper a)
genListZipper g =
  do  l <- Gen.list (Range.linear 0 100) g
      x <- g
      r <- Gen.list (Range.linear 0 100) g
      pure (ListZipper l x r)

listzipper_properties ::
  TestTree
listzipper_properties =
  testGroup "ListZipper"
    [
      testProperty "movement does not edit" prop_movementDoesNotEdit'
    , testProperty "move right then delete" prop_moveRightThenDelete'
    , testProperty "move left then delete" prop_moveLeftThenDelete'
    ]

noeditOperation' ::
  (a -> Bool)
  -> Gen (ListZipperOp' a, String)
noeditOperation' f =
  Gen.element
    [
      (moveLeft, "moveLeft")
    , (moveRight, "moveRight")
    , (moveLeftUntil f, "moveLeftUntil")
    , (moveRightUntil f, "moveRightUntil")
    , (moveLeftRightUntil f, "moveLeftRightUntil")
    , (moveRightLeftUntil f, "moveRightLeftUntil")
    , (moveLeftUntilThen f, "moveLeftUntilThen")
    , (moveRightUntilThen f, "moveRightUntilThen")
    , (moveLeftRightUntilThen f, "moveLeftRightUntilThen")
    , (moveRightLeftUntilThen f, "moveRightLeftUntilThen")
    ]

prop_movementDoesNotEdit ::
  forall a.
  (Show a, Eq a, Arg a, Vary a) =>
  Gen a
  -> Property
prop_movementDoesNotEdit genA =
  property $
    do  f      <- forAllFn (fn @a Gen.bool)
        (o, _) <- forAllWith (\(_, s) -> s) (noeditOperation' f)
        z      <- forAll (genListZipper genA)
        traverse_ (\z' -> list z === list z') (o ##> z)

prop_movementDoesNotEdit' ::
  Property
prop_movementDoesNotEdit' =
  prop_movementDoesNotEdit (Gen.int (Range.linear 0 9999))

prop_moveRightThenDelete ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveRightThenDelete genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (moveRight *> deleteStepLeft) `runListZipperOp` z
        traverse_ (\(ListZipper l x r, v) -> ListZipper l x (v:r) === z) t

prop_moveRightThenDelete' ::
  Property
prop_moveRightThenDelete' =
  prop_moveRightThenDelete (Gen.int (Range.linear 0 9999))

prop_moveLeftThenDelete ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveLeftThenDelete genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (moveLeft *> deleteStepRight) `runListZipperOp` z
        traverse_ (\(ListZipper l x r, v) -> ListZipper (v:l) x r === z) t

prop_moveLeftThenDelete' ::
  Property
prop_moveLeftThenDelete' =
  prop_moveLeftThenDelete (Gen.int (Range.linear 0 9999))
