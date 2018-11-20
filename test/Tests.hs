{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative(pure, (*>), (<*))
import Control.Category((.))
import Control.Lens((^.))
import Control.Monad(replicateM_)
import Control.Monad.State(modify)
import Data.Bool(Bool(True, False))
import Data.Eq(Eq)
import Data.Foldable(traverse_)
import Data.Function(($))
import Data.Functor((<$>))
import Data.List(zip)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.ListZipper(ListZipper(ListZipper), ListZipperOp', moveLeft, moveRight, moveLeftUntil, moveRightUntil, moveLeftRightUntil, moveRightLeftUntil, moveLeftUntilThen, moveRightUntilThen, moveLeftRightUntilThen, moveRightLeftUntilThen, list, (##>), deleteStepLeft, deleteStepRight, runListZipperOp, execListZipperOp, moveEnd, moveStart, atStart, atEnd, zipper0L, zipper0L', moveLeftLoop, moveRightLoop, insertMoveLeft, insertMoveRight, focus, zipperIndices, moveRightWith, moveLeftWith, moveLeftRightWith, moveRightLeftWith, moveRightWithThen, moveLeftWithThen)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String)
import Hedgehog(Gen, Property, property, forAll, forAllWith, (===))
import Hedgehog.Function(Arg, Vary, forAllFn, fn)
import qualified Hedgehog.Gen as Gen(list, element, bool, int, maybe, alpha)
import qualified Hedgehog.Range as Range(linear)
import Prelude(Show)
import System.IO(IO)
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog(testProperty)

main ::
  IO ()
main =
  defaultMain listzipper_properties

listzipper_properties ::
  TestTree
listzipper_properties =
  testGroup "ListZipper"
    [
      testProperty "movement does not edit" prop_movementDoesNotEdit'
    , testProperty "move right then delete" prop_moveRightThenDelete'
    , testProperty "move left then delete" prop_moveLeftThenDelete'
    , testProperty "move right then move left is identity" prop_moveRight_moveLeft'
    , testProperty "move left then move right is identity" prop_moveLeft_moveRight'
    , testProperty "move end cannot move right" prop_moveEnd_cannot_moveRight'
    , testProperty "move start cannot move left" prop_moveStart_cannot_moveLeft'
    , testProperty "move start is at start" prop_moveStart_atStart'
    , testProperty "move end is at end" prop_moveEnd_atEnd'
    , testProperty "move start then move right is not at start" prop_moveStart_moveRight_not_atStart'
    , testProperty "move end then move left is not at end" prop_moveEnd_moveLeft_not_atEnd'
    , testProperty "move zipper0L rezip is same list" prop_zipper0L_list'
    , testProperty "move zipper0L' rezip is same list" prop_zipper0L'_list'
    , testProperty "moveLeftLoop start is at end" prop_moveLeftLoop_start_atEnd'
    , testProperty "moveRightLoop end is at start" prop_moveRightLoop_end_atStart'
    , testProperty "insertMoveLeft gets correct focus" prop_insertMoveLeft_focus'
    , testProperty "insertMoveLeft gets correct focus" prop_insertMoveRight_focus'
    , testProperty "zipperIndices are linear" prop_indices'
    , testProperty "moveRightWith focus satisfies" prop_moveRightWith_focus'
    , testProperty "moveLeftWith focus satisfies" prop_moveLeftWith_focus'
    , testProperty "moveLeftRightWith focus satisfies" prop_moveLeftRightWith_focus'
    , testProperty "moveRightLeftWith focus satisfies" prop_moveRightLeftWith_focus'
    , testProperty "moveLeftWithThen focus satisfies" prop_moveLeftWithThen_focus'
    , testProperty "moveRightWithThen focus satisfies" prop_moveRightWithThen_focus'
    ]

genListZipper ::
  Gen a
  -> Gen (ListZipper a)
genListZipper g =
  do  l <- Gen.list (Range.linear 0 100) g
      x <- g
      r <- Gen.list (Range.linear 0 100) g
      pure (ListZipper l x r)

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

prop_moveRight_moveLeft ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveRight_moveLeft genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (moveRight *> moveLeft) `execListZipperOp` z
        traverse_ (=== z) t

prop_moveRight_moveLeft' ::
  Property
prop_moveRight_moveLeft' =
  prop_moveRight_moveLeft (Gen.int (Range.linear 0 9999))

prop_moveLeft_moveRight ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveLeft_moveRight genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (moveLeft *> moveRight) `execListZipperOp` z
        traverse_ (=== z) t

prop_moveLeft_moveRight' ::
  Property
prop_moveLeft_moveRight' =
  prop_moveLeft_moveRight (Gen.int (Range.linear 0 9999))

prop_moveEnd_cannot_moveRight ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveEnd_cannot_moveRight genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (modify moveEnd *> moveRight) `execListZipperOp` z
        t === Nothing

prop_moveEnd_cannot_moveRight' ::
  Property
prop_moveEnd_cannot_moveRight' =
  prop_moveEnd_cannot_moveRight (Gen.int (Range.linear 0 9999))

prop_moveStart_cannot_moveLeft ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveStart_cannot_moveLeft genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = (modify moveStart *> moveLeft) `execListZipperOp` z
        t === Nothing

prop_moveStart_cannot_moveLeft' ::
  Property
prop_moveStart_cannot_moveLeft' =
  prop_moveStart_cannot_moveLeft (Gen.int (Range.linear 0 9999))

prop_moveStart_atStart ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveStart_atStart genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = modify moveStart `execListZipperOp` z
        (atStart <$> t) === Just True

prop_moveStart_atStart' ::
  Property
prop_moveStart_atStart' =
  prop_moveStart_atStart (Gen.int (Range.linear 0 9999))

prop_moveEnd_atEnd ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveEnd_atEnd genA =
  property $
    do  z <- forAll (genListZipper genA)
        let t = modify moveEnd `execListZipperOp` z
        (atEnd <$> t) === Just True

prop_moveEnd_atEnd' ::
  Property
prop_moveEnd_atEnd' =
  prop_moveEnd_atEnd (Gen.int (Range.linear 0 9999))

prop_moveStart_moveRight_not_atStart ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveStart_moveRight_not_atStart genA =
  property $
    do  z <- forAll (genListZipper genA)
        n <- forAll (Gen.int (Range.linear 1 99))
        let t = (modify moveStart *> replicateM_ n moveRight) `execListZipperOp` z
        traverse_ (\z' -> atStart z' === False) t

prop_moveStart_moveRight_not_atStart' ::
  Property
prop_moveStart_moveRight_not_atStart' =
  prop_moveStart_moveRight_not_atStart (Gen.int (Range.linear 0 9999))

prop_moveEnd_moveLeft_not_atEnd ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveEnd_moveLeft_not_atEnd genA =
  property $
    do  z <- forAll (genListZipper genA)
        n <- forAll (Gen.int (Range.linear 1 9999))
        let t = (modify moveEnd *> replicateM_ n moveLeft) `execListZipperOp` z
        traverse_ (\z' -> atEnd z' === False) t

prop_moveEnd_moveLeft_not_atEnd' ::
  Property
prop_moveEnd_moveLeft_not_atEnd' =
  prop_moveEnd_moveLeft_not_atEnd (Gen.int (Range.linear 0 9999))

prop_zipper0L_list ::
  forall a.
  (Eq a, Show a, Vary a, Arg a) =>
  Gen a
  -> Property
prop_zipper0L_list genA =
  property $
    do  hd     <- forAll genA
        tl     <- forAll (Gen.list (Range.linear 0 100) genA)
        f      <- forAllFn (fn @a Gen.bool)
        (o, _) <- forAllWith (\(_, s) -> s) (noeditOperation' f)
        n      <- forAll (Gen.int (Range.linear 1 99))
        let t = list <$> replicateM_ n o `execListZipperOp` (zipper0L hd tl)
        traverse_ (=== hd:tl) t
        
prop_zipper0L_list' ::
  Property
prop_zipper0L_list' =
  prop_zipper0L_list (Gen.int (Range.linear 0 9999))

prop_zipper0L'_list ::
  forall a.
  (Eq a, Show a, Vary a, Arg a) =>
  Gen a
  -> Property
prop_zipper0L'_list genA =
  property $
    do  hd     <- forAll genA
        tl     <- forAll (Gen.list (Range.linear 0 100) genA)
        f      <- forAllFn (fn @a Gen.bool)
        (o, _) <- forAllWith (\(_, s) -> s) (noeditOperation' f)
        n      <- forAll (Gen.int (Range.linear 1 99))
        let t = list <$> replicateM_ n o `execListZipperOp` (zipper0L' (hd :| tl))
        traverse_ (=== hd:tl) t
        
prop_zipper0L'_list' ::
  Property
prop_zipper0L'_list' =
  prop_zipper0L'_list (Gen.int (Range.linear 0 9999))

prop_moveLeftLoop_start_atEnd ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveLeftLoop_start_atEnd genA =
  property $
    do  z <- forAll (genListZipper genA)
        atEnd ((moveLeftLoop . moveStart) z) === True

prop_moveLeftLoop_start_atEnd' ::
  Property
prop_moveLeftLoop_start_atEnd' =
  prop_moveLeftLoop_start_atEnd (Gen.int (Range.linear 0 9999))

prop_moveRightLoop_end_atStart ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_moveRightLoop_end_atStart genA =
  property $
    do  z <- forAll (genListZipper genA)
        atStart ((moveRightLoop . moveEnd) z) === True

prop_moveRightLoop_end_atStart' ::
  Property
prop_moveRightLoop_end_atStart' =
  prop_moveRightLoop_end_atStart (Gen.int (Range.linear 0 9999))

prop_insertMoveLeft_focus ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_insertMoveLeft_focus genA =
  property $
    do  a <- forAll genA
        z <- forAll (genListZipper genA)
        insertMoveLeft a z ^. focus === a

prop_insertMoveLeft_focus' ::
  Property
prop_insertMoveLeft_focus' =
  prop_insertMoveLeft_focus (Gen.int (Range.linear 0 9999))

prop_insertMoveRight_focus ::
  (Eq a, Show a) =>
  Gen a
  -> Property
prop_insertMoveRight_focus genA =
  property $
    do  a <- forAll genA
        z <- forAll (genListZipper genA)
        insertMoveRight a z ^. focus === a

prop_insertMoveRight_focus' ::
  Property
prop_insertMoveRight_focus' =
  prop_insertMoveRight_focus (Gen.int (Range.linear 0 9999))

prop_indices ::
  forall a.
  (Eq a, Show a, Arg a, Vary a) =>
  Gen a
  -> Property
prop_indices genA =
  property $
    do  z <- forAll (genListZipper genA)
        f      <- forAllFn (fn @a Gen.bool)
        (o, _) <- forAllWith (\(_, s) -> s) (noeditOperation' f)
        n      <- forAll (Gen.int (Range.linear 1 99))
        let t = list . zipperIndices <$> replicateM_ n o `execListZipperOp` z
        traverse_ (=== zip [0..] (list z)) t

prop_indices' ::
  Property
prop_indices' =
  prop_indices (Gen.int (Range.linear 0 9999))

prop_moveRightWith_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveRightWith_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = moveRightWith f `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveRightWith_focus' ::
  Property
prop_moveRightWith_focus' =
  prop_moveRightWith_focus (Gen.int (Range.linear 0 99)) (Gen.int (Range.linear 0 99))

prop_moveLeftWith_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveLeftWith_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = moveLeftWith f `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveLeftWith_focus' ::
  Property
prop_moveLeftWith_focus' =
  prop_moveLeftWith_focus (Gen.int (Range.linear 0 99)) (Gen.int (Range.linear 0 99))

prop_moveLeftRightWith_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveLeftRightWith_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = moveLeftRightWith f `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveLeftRightWith_focus' ::
  Property
prop_moveLeftRightWith_focus' =
  prop_moveLeftRightWith_focus (Gen.int (Range.linear 0 99)) (Gen.int (Range.linear 0 99))

prop_moveRightLeftWith_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveRightLeftWith_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = moveRightLeftWith f `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveRightLeftWith_focus' ::
  Property
prop_moveRightLeftWith_focus' =
  prop_moveRightLeftWith_focus (Gen.int (Range.linear 0 99)) (Gen.int (Range.linear 0 99))

prop_moveLeftWithThen_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveLeftWithThen_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = (moveLeftWithThen f <* moveRight) `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveLeftWithThen_focus' ::
  Property
prop_moveLeftWithThen_focus' =
  prop_moveLeftWithThen_focus (Gen.int (Range.linear 0 99)) (Gen.list (Range.linear 0 99) Gen.alpha)

prop_moveRightWithThen_focus ::
  forall a c.
  (Eq a, Show a, Arg a, Vary a, Show c, Eq c) =>
  Gen a
  -> Gen c
  -> Property
prop_moveRightWithThen_focus genA genC =
  property $
    do  z <- forAll (genListZipper genA)
        f <- forAllFn (fn @a (Gen.maybe genC))
        let t = (moveRightWithThen f <* moveLeft) `runListZipperOp` z
        traverse_ (\(z', x) -> Just x === (f (z' ^. focus))) t

prop_moveRightWithThen_focus' ::
  Property
prop_moveRightWithThen_focus' =
  prop_moveRightWithThen_focus (Gen.int (Range.linear 0 99)) (Gen.list (Range.linear 0 99) Gen.alpha)
