{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListZipper {-(
  ListZipper(..)
, ListZipperOp(..)
, pureListZipperOp
, unpureListZipperOp
, constListZipperOp
, idListZipperOp
, (<||>)
, (&^.)
, moveLeft
, moveRight
, opUntil
, moveLeftUntil
, moveRightUntil
, moveLeftRightUntil
, moveRightLeftUntil
, opWhileJust
, moveStart
, moveEnd
, atStart
, atEnd
, moveLeftLoop
, moveRightLoop
, deleteStepLeft
, deleteStepRight
, insertMoveLeft
, insertMoveRight
, AsListZipper(..)
, HasListZipper(..)
, leftz'
, rightz'
, leftzrightz
, rightzleftz
, zipper
, zipper0L
, zipper0L'
, zipper0R
, zipper0R'
, list
, zipperIndices
) -} where

import Control.Applicative(Applicative(pure, (<*>)), Alternative((<|>), empty))
import Control.Category((.), id)
import Control.Comonad(Comonad(duplicate, extract))
import Control.Lens hiding ((<.>))
import Control.Monad(Monad((>>=), return), MonadPlus(mplus, mzero), (>=>), (=<<))
import Data.Bool(Bool)
import Data.Eq(Eq((==)))
import Data.Eq.Deriving(deriveEq1)
import Data.Foldable(Foldable(toList, foldMap))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Int(Int)
import Data.List(unfoldr, zipWith, repeat, reverse, null, zip)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Nothing, Just), fromMaybe)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<)))
import Data.Semigroup(Semigroup((<>)))
import Data.Semigroup.Foldable(Foldable1(foldMap1))
import Prelude(Show, (+))
import Text.Show.Deriving(deriveShow1)

import Data.Profunctor
import Data.Either(Either(Left, Right), either)
import Data.Traversable

newtype LO x y =
  LO (ListZipper x -> Maybe y)

instance LO x y ~ t =>
  Rewrapped (LO x' y') t

instance Wrapped (LO x' y') where
  type Unwrapped (LO x' y') =
    ListZipper x'
    -> Maybe y'
  _Wrapped' =
    iso (\(LO k) -> k) LO

class HasLO lo x y | lo -> x y where
  lo ::
    Lens' lo (LO x y)

instance HasLO (LO x y) x y where
  lo =
    id

class AsLO t x y | t -> x y where
  _LO :: Prism' t (LO x y)

instance AsLO (LO x y) x y where
  _LO =
    id
    
undefined = undefined

instance Functor (LO x) where
  fmap f (LO k) =
    LO (fmap f . k)

instance Apply (LO x) where
  LO j <.> LO k =
    LO (\z -> j z <.> k z)

instance Applicative (LO x) where
  (<*>) =
    (<.>)
  pure =
    LO . pure . pure

instance Bind (LO x) where
  LO j >>- f =
    LO (\z -> j z >>- \a -> let LO k = f a in k z)

instance Alt (LO x) where
  LO j <!> LO k =
    LO (\z -> j z <!> k z)

instance Alternative (LO x) where
  (<|>) =
    (<!>)
  empty =
    LO (pure empty)

instance Monad (LO x) where
  (>>=) =
    (>>-)
  return =
    pure

instance MonadPlus (LO x) where
  LO j `mplus` LO k =
    LO (\z -> j z `mplus` k z)
  mzero =
    LO (pure mzero)

instance Semigroup (LO x y) where
  LO j <> LO k =
    LO (\z -> j z <!> k z)

instance Monoid (LO x y) where
  mappend =
    (<>)
  mempty =
    LO (pure Nothing)

instance Profunctor LO where
  dimap f g (LO k) =
    LO (fmap g . k . fmap f)
  rmap =
    fmap

----

data ListZipper a =
  ListZipper 
    [a]
    a
    [a]
  deriving (Eq, Ord, Show)

instance Functor ListZipper where
  fmap f (ListZipper l x r) =
    ListZipper (fmap f l) (f x) (fmap f r)

instance Apply ListZipper where
  ListZipper l1 x1 r1 <.> ListZipper l2 x2 r2 =
    ListZipper (zipWith id l1 l2) (x1 x2) (zipWith id r1 r2)

instance Applicative ListZipper where
  pure a =
    ListZipper (repeat a) a (repeat a)
  (<*>) =
    (<.>)

instance Foldable ListZipper where
  foldMap f (ListZipper l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

instance Foldable1 ListZipper where
  foldMap1 f (ListZipper [] x []) =
    f x
  foldMap1 f (ListZipper [] x (rh:rt)) =
    f x <> foldMap1 f (rh :| rt)
  foldMap1 f (ListZipper (lh:lt) x []) =
    foldMap1 f (lh :| lt) <> f x
  foldMap1 f (ListZipper (lh:lt) x (rh:rt)) =
    foldMap1 f (lh :| lt) <> f x <> foldMap1 f (rh :| rt)

instance Traversable ListZipper where
  traverse f (ListZipper l x r) =
    ListZipper <$> traverse f l <*> f x <*> traverse f r

instance Traversable1 ListZipper where
  traverse1 f (ListZipper [] x []) =
    (\x' -> ListZipper [] x' []) <$> f x
  traverse1 f (ListZipper (lh:lt) x []) =
    (\l' x' -> ListZipper (toList l') x' []) <$> traverse1 f (lh :| lt) <.> f x
  traverse1 f (ListZipper [] x (rh:rt)) =
    (\x' r' -> ListZipper [] x' (toList r')) <$> f x <.> traverse1 f (rh :| rt)
  traverse1 f (ListZipper (lh:lt) x (rh:rt)) =
    (\l' x' r' -> ListZipper (toList l') x' (toList r')) <$> traverse1 f (lh :| lt) <.> f x <.> traverse1 f (rh :| rt)

instance Semigroup a => Semigroup (ListZipper a) where
  ListZipper l1 x1 r1 <> ListZipper l2 x2 r2 =
    ListZipper (zipWith (<>) l1 l2) (x1 <> x2) (zipWith (<>) r1 r2)

instance Each (ListZipper a) (ListZipper a) a a where
  each =
    traverse

instance Reversing (ListZipper a) where
  reversing (ListZipper l x r) =
    ListZipper (reverse l) x (reverse r)

type instance IxValue (ListZipper a) = a
type instance Index (ListZipper a) = Int
instance Ixed (ListZipper a) where
  ix i f z =
    if i < 0
      then
        pure z
      else
        let ListZipper l x r =
              zipperIndices z
            applyn (n, a) =
              if i == n
                then
                  f a
                else
                  pure a
        in  ListZipper <$> traverse applyn l <*> applyn x <*> traverse applyn r

instance Extend ListZipper where
  duplicated z =
    let dup x =
          (x, x)
        unf m =
          unfoldr (fmap dup . (m ^. _Wrapped)) z
    in  ListZipper (unf moveLeft) z (unf moveRight)

instance Comonad ListZipper where
  duplicate =
    duplicated
  extract (ListZipper _ x _) =
    x

newtype ListZipperOp a =
  ListZipperOp
    (ListZipper a -> Maybe (ListZipper a))

instance ListZipperOp x ~ y =>
  Rewrapped (ListZipperOp w) y

instance Wrapped (ListZipperOp x) where
  type Unwrapped (ListZipperOp x) =
    ListZipper x
    -> Maybe (ListZipper x)
  _Wrapped' =
    iso
      (\(ListZipperOp x) -> x)
      ListZipperOp

instance Semigroup (ListZipperOp a) where
  ListZipperOp x <> ListZipperOp y =
    ListZipperOp (x >=> y)

instance Monoid (ListZipperOp a) where
  mappend =
    (<>)
  mempty =
    ListZipperOp (pure Nothing)

pureListZipperOp ::
  (ListZipper a -> ListZipper a)
  -> ListZipperOp a
pureListZipperOp k =
  ListZipperOp (Just . k)

unpureListZipperOp ::
  ListZipperOp a
  -> ListZipper a
  -> ListZipper a
unpureListZipperOp (ListZipperOp x) =
  fromMaybe <*> x

constListZipperOp ::
  ListZipper a
  -> ListZipperOp a
constListZipperOp =
  pureListZipperOp . pure

idListZipperOp ::
  ListZipperOp a
idListZipperOp =
  ListZipperOp Just

(<||>) ::
  ListZipperOp a
  -> ListZipperOp a
  -> ListZipperOp a
ListZipperOp x <||> ListZipperOp y =
  ListZipperOp (\z ->
    x z <!> y z  
  )

infixl 3 <||>

(&^.) ::
  ListZipperOp a
  -> ListZipper a
  -> Maybe (ListZipper a)
(&^.) o z =
  z & o ^. _Wrapped

infixr 5 &^.

moveLeft ::
  ListZipperOp a
moveLeft =
  ListZipperOp (\z ->
    case z of
      ListZipper [] _ _ ->
        Nothing
      ListZipper (h:t) x r ->
        Just (ListZipper t h (x:r))
  )

moveRight ::
  ListZipperOp a
moveRight =
  ListZipperOp (\z ->
    case z of
      ListZipper _ _ [] ->
        Nothing
      ListZipper l x (h:t) ->
        Just (ListZipper (x:l) h t)
  )

opUntil ::
  ListZipperOp a
  -> (a -> Bool)
  -> ListZipperOp a
opUntil o p =
  ListZipperOp (\z ->
    let go z' =
          let x = z' ^. focus
          in  if p x
                then
                  Just z'
                else
                  go =<< o &^. z'
    in  go z
  )

moveLeftUntil ::
  (a -> Bool)
  -> ListZipperOp a
moveLeftUntil =
  opUntil moveLeft

moveRightUntil ::
  (a -> Bool)
  -> ListZipperOp a
moveRightUntil =
  opUntil moveRight

moveLeftRightUntil ::
  (a -> Bool)
  -> ListZipperOp a
moveLeftRightUntil p =
  moveLeftUntil p <||> moveRightUntil p

moveRightLeftUntil ::
  (a -> Bool)
  -> ListZipperOp a
moveRightLeftUntil p =
  moveRightUntil p <||> moveLeftUntil p

opWhileJust ::
  ListZipperOp a
  -> ListZipper a
  -> ListZipper a
opWhileJust o z =
  case o &^. z of
    Nothing ->
      z
    Just z' ->
      opWhileJust o z'

moveStart ::
  ListZipper a
  -> ListZipper a
moveStart =
  opWhileJust moveLeft

moveEnd ::
  ListZipper a
  -> ListZipper a
moveEnd =
  opWhileJust moveRight

atStart ::
  HasListZipper z a =>
  z
  -> Bool
atStart z =
  null (z ^. leftz)

atEnd ::
  HasListZipper z a =>
  z
  -> Bool
atEnd z =
  null (z ^. rightz)

moveLeftLoop ::
  ListZipper a
  -> ListZipper a
moveLeftLoop z =
  fromMaybe (moveEnd z) (moveLeft &^. z)

moveRightLoop ::
  ListZipper a
  -> ListZipper a
moveRightLoop z =
  fromMaybe (moveStart z) (moveRight &^. z)

deleteStepLeft ::
  ListZipperOp a
deleteStepLeft =
  ListZipperOp (\z ->
    let l = z ^. leftz
        r = z ^. rightz
    in  case l of
          [] ->
            Nothing
          h:t ->
            Just (ListZipper t h r)
  )

deleteStepRight ::
  ListZipperOp a
deleteStepRight =
  ListZipperOp (\z ->
    let l = z ^. leftz
        r = z ^. rightz
    in  case r of
          [] ->
            Nothing
          h:t ->
            Just (ListZipper l h t)
  )

insertMoveLeft ::
  a
  -> ListZipper a
  -> ListZipper a
insertMoveLeft a (ListZipper l x r) =
  ListZipper (a:l) x r

insertMoveRight ::
  a
  -> ListZipper a
  -> ListZipper a
insertMoveRight a (ListZipper l x r) =
  ListZipper l x (a:r)

class AsListZipper z a | z -> a where
  _ListZipper ::
    Prism' z (ListZipper a)
  
instance AsListZipper (ListZipper a) a where
  _ListZipper =
    id

class HasListZipper z a | z -> a where
  listZipper ::
    Lens' z (ListZipper a)
  focus ::
    Lens' z a
  {-# INLINE focus #-}
  leftz ::
    Lens' z [a]
  {-# INLINE leftz #-}
  rightz ::
    Lens' z [a]
  {-# INLINE rightz #-}
  leftz =
    listZipper . leftz
  focus =
    listZipper . focus
  rightz =
    listZipper . rightz

instance HasListZipper (ListZipper a) a where
  {-# INLINE focus #-}
  {-# INLINE leftz #-}
  {-# INLINE rightz #-}
  listZipper =
    id
  leftz f (ListZipper l x r) =
    fmap (\l' -> ListZipper l' x r) (f l)
  focus f (ListZipper l x r) =
    fmap (\x' -> ListZipper l x' r) (f x)
  rightz f (ListZipper l x r) =
    fmap (\r' -> ListZipper l x r') (f r)

leftz' ::
  HasListZipper z a =>
  Traversal' z a
leftz' =
  leftz . traverse

rightz' ::
  HasListZipper z a =>
  Traversal' z a
rightz' =
  rightz . traverse

leftzrightz ::
  Traversal' (ListZipper a) a
leftzrightz f (ListZipper l x r) =
  (\l' r' -> ListZipper l' x r') <$> traverse f l <*> traverse f r

rightzleftz ::
  Traversal' (ListZipper a) a
rightzleftz f (ListZipper l x r) =
  (\r' l' -> ListZipper l' x r') <$> traverse f r <*> traverse f l

zipper ::
  [a]
  -> Maybe (ListZipper a)
zipper [] =
  Nothing
zipper (h:t) =
  Just (ListZipper [] h t)

zipper0L ::
  a
  -> [a]
  -> ListZipper a
zipper0L =
  ListZipper []

zipper0L' ::
  NonEmpty a
  -> ListZipper a
zipper0L' (h :| t) =
  zipper0L h t

zipper0R ::
  [a]
  -> a
  -> ListZipper a
zipper0R l x =
  ListZipper l x []

zipper0R' ::
  NonEmpty a
  -> ListZipper a
zipper0R' (h :| []) =
  ListZipper [] h []
zipper0R' (h :| i : t) =
  let ListZipper l x r = zipper0R' (i :| t)
  in  ListZipper (h:l) x r

list ::
  ListZipper a
  -> [a]
list (ListZipper l x r) =
  reverse l <> (x : r)

zipperIndices ::
  ListZipper a
  -> ListZipper (Int, a)
zipperIndices (ListZipper l x r) =
  let zipl ::
        [a]
        -> [b]
        -> ([a], [(a, b)])
      zipl y [] =
        (y, [])
      zipl [] (_:_) =
        ([], [])
      zipl (a:b) (c:d) =
        fmap ((a, c) :) (zipl b d)
      rl =
        reverse l
      (z, l') =
        zipl [0..] rl
      ln =
        case z of
          n:_ ->
            n
          [] ->
            0
  in  ListZipper
        (reverse l')
        (ln, x)
        (zip [ln + 1..] r)

deriveEq1 ''ListZipper
deriveShow1 ''ListZipper
