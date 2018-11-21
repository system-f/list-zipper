{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListZipper(
-- * data types
  ListZipper(..)
, ListZipperOp(..)
, ListZipperOp'
-- * lenses and prisms
, AsListZipper(..)
, HasListZipper(..)
, HasListZipperOp(..)
, AsListZipperOp(..)
-- * traversals
, leftz'
, rightz'
, leftzrightz
, rightzleftz
-- * make zippers
, zipper
, zipper0L
, zipper0L'
, zipper0R
, zipper0R'
-- * rezip
, list
-- * indices
, zipperIndices
-- * movement
, moveLeft
, moveRight
, moveStart
, moveEnd
, moveLeftLoop
, moveRightLoop
, opWith
, moveLeftWith
, moveRightWith
, moveLeftRightWith
, moveRightLeftWith
, opWithThen
, moveLeftWithThen
, moveRightWithThen
, moveLeftRightWithThen
, moveRightLeftWithThen
, opUntil
, moveLeftUntil
, moveRightUntil
, moveLeftRightUntil
, moveRightLeftUntil
, opUntilThen
, moveLeftUntilThen
, moveRightUntilThen
, moveLeftRightUntilThen
, moveRightLeftUntilThen
-- * insertion
, insertMoveLeft
, insertMoveRight
-- * deletion
, deleteStepLeft
, deleteStepRight
-- * modifcation
, modifyFocus
, setFocus
-- * focus position
, atStart
, atEnd
-- * get context
, getFocus
, getLeft
, getRight
, getRightz
, getLeftz
, getList
-- * list zipper state operations
, liftListZipperOp
, mkListZipperOp
, (*>>)
, (<<*)
, mkListZipperOp'
, (.>>)
, (<<.)
, runListZipperOp
, execListZipperOp
, (##>)
, (<##)
, evalListZipperOp
, (&&>)
, (<&&)
, execOpList
, (%%>)
, (<%%)
, execOpList'
, ($$>)
, (<$$)
, opWhileJust
) where

import Control.Applicative(Applicative(pure, (<*>)), Alternative((<|>), empty), (<*))
import Control.Category((.), id)
import Control.Comonad(Comonad(duplicate, extract))
import Control.Lens(Each(each), Reversing(reversing), Ixed(ix), Rewrapped, Wrapped(Unwrapped, _Wrapped'), IxValue, Index, Prism', Lens', Traversal', _Wrapped, (^.), iso, (&), _1, _2)
import Control.Monad.Error.Class(MonadError(throwError, catchError))
import Control.Monad.Fail(MonadFail(fail))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.Reader(MonadReader(ask, local, reader))
import Control.Monad.State(MonadState(get, put, state))
import qualified Control.Monad.Fail as Fail(fail)
import Data.Traversable(Traversable(traverse))
import Data.Semigroup.Traversable(Traversable1(traverse1))
import Control.Monad(Monad((>>=), return), MonadPlus(mplus, mzero), (=<<))
import Data.Bool(Bool)
import Data.Eq(Eq((==)))
import Data.Eq.Deriving(deriveEq1)
import Data.Foldable(Foldable(toList, foldMap))
import Data.Function(flip)
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Alt(Alt((<!>)))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Extend(Extend(duplicated))
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
          unfoldr (fmap dup . (execListZipperOp m)) z
    in  ListZipper (unf moveLeft) z (unf moveRight)

instance Comonad ListZipper where
  duplicate =
    duplicated
  extract (ListZipper _ x _) =
    x

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

moveStart ::
  ListZipper a
  -> ListZipper a
moveStart =
  opWhileJust moveLeft

moveEnd ::
  ListZipper a
  -> ListZipper a
moveEnd z =
  opWhileJust moveRight z

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
  fromMaybe (moveEnd z) (execListZipperOp moveLeft z)

moveRightLoop ::
  ListZipper a
  -> ListZipper a
moveRightLoop z =
  fromMaybe (moveStart z) (execListZipperOp moveRight z)

insertMoveLeft ::
  a
  -> ListZipper a
  -> ListZipper a
insertMoveLeft a (ListZipper l x r) =
  ListZipper (x:l) a r

insertMoveRight ::
  a
  -> ListZipper a
  -> ListZipper a
insertMoveRight a (ListZipper l x r) =
  ListZipper l a (x:r)

----

newtype ListZipperOp a b =
  ListZipperOp (ListZipper a -> Maybe (ListZipper a, b))

type ListZipperOp' a =
  ListZipperOp a ()

instance ListZipperOp a x ~ t =>
  Rewrapped (ListZipperOp b' a') t

instance Wrapped (ListZipperOp a' x') where
  type Unwrapped (ListZipperOp a' x') =
    ListZipper a'
    -> Maybe (ListZipper a', x')
  _Wrapped' =
    iso (\(ListZipperOp k) -> k) ListZipperOp

class HasListZipperOp lo x y | lo -> x y where
  lo ::
    Lens' lo (ListZipperOp x y)

instance HasListZipperOp (ListZipperOp x y) x y where
  lo =
    id

class AsListZipperOp t x y | t -> x y where
  _ListZipperOp :: Prism' t (ListZipperOp x y)

instance AsListZipperOp (ListZipperOp x y) x y where
  _ListZipperOp =
    id

instance Functor (ListZipperOp a) where
  fmap f (ListZipperOp k) =
    ListZipperOp (fmap (fmap f) . k)

instance Apply (ListZipperOp a) where
  ListZipperOp j <.> ListZipperOp k =
    ListZipperOp (\z ->
      j z >>= \(z', f) ->
      k z' >>= \(z'', a) ->
      pure (z'', f a)
      )

instance Applicative (ListZipperOp a) where
  (<*>) =
    (<.>)
  pure a =
    ListZipperOp (\z -> pure (z, a))

instance Bind (ListZipperOp a) where
  ListZipperOp j >>- f =
    ListZipperOp (\z -> 
      j z >>- \(z', a) ->
      z' & f a ^. _Wrapped
      )

instance Alt (ListZipperOp a) where
  ListZipperOp j <!> ListZipperOp k =
    ListZipperOp (\z -> j z <!> k z)

instance Alternative (ListZipperOp x) where
  (<|>) =
    (<!>)
  empty =
    ListZipperOp (pure empty)

instance Monad (ListZipperOp a) where
  (>>=) =
    (>>-)
  return =
    pure

instance MonadPlus (ListZipperOp a) where
  ListZipperOp j `mplus` ListZipperOp k =
    ListZipperOp (\z -> j z `mplus` k z)
  mzero =
    ListZipperOp (pure mzero)

instance Semigroup (ListZipperOp a b) where
  ListZipperOp j <> ListZipperOp k =
    ListZipperOp (\z -> j z <!> k z)

instance Monoid (ListZipperOp a b) where
  mappend =
    (<>)
  mempty =
    ListZipperOp (pure Nothing)

instance MonadState (ListZipper a) (ListZipperOp a) where
  get =
    ListZipperOp (\z -> Just (z, z))
  put z =
    ListZipperOp (\_ -> Just (z, ()))
  state k =
    ListZipperOp (\z -> let (z', a) = k z in Just (a, z'))

instance MonadReader (ListZipper a) (ListZipperOp a) where
  ask =
    ListZipperOp (\z -> Just (z, z))
  local k (ListZipperOp o) =
    ListZipperOp (\z -> (\(z', a) -> (k z', a)) <$> o z)
  reader k =
    ListZipperOp (\z -> Just (z, k z))

instance MonadFix (ListZipperOp a) where
  mfix f =
    ListZipperOp (\z ->
      mfix (\ ~(_, a) -> z & f a ^. _Wrapped)
      )

instance MonadFail (ListZipperOp a) where
  fail s =
    ListZipperOp (\_ ->
      Fail.fail s
    )

instance MonadError () (ListZipperOp a) where
  throwError () =
    ListZipperOp (\_ -> Nothing)
  catchError (ListZipperOp k) f =
    ListZipperOp (\z ->
       k z <!> (z & f () ^. _Wrapped)
     )

liftListZipperOp ::
  Maybe b
  -> ListZipperOp a b
liftListZipperOp m =
  ListZipperOp (\z ->
    (\b -> (z, b)) <$> m
  )

getFocus ::
  ListZipperOp a a
getFocus =
  reader (^. focus)

getLeft ::
  ListZipperOp a a
getLeft =
  do  z <- get
      case z of
        ListZipper (l:_) _ _ ->
          pure l
        ListZipper [] _ _ ->
          mempty

getRight ::
  ListZipperOp a a
getRight =
  do  z <- get
      case z of
        ListZipper _ _ (r:_) ->
          pure r
        ListZipper _ _ [] ->
          mempty

getRightz ::
  ListZipperOp a [a]
getRightz =
  reader (^. rightz)

getLeftz ::
  ListZipperOp a [a]
getLeftz =
  reader (^. leftz)

getList ::
  ListZipperOp a [a]
getList =
  reader list

mkListZipperOp :: 
  (ListZipper a -> Maybe b)
  -> ListZipperOp a b
mkListZipperOp f = 
  get >>= liftListZipperOp . f

(*>>) :: 
  (ListZipper a -> Maybe b)
  -> ListZipperOp a c
  -> ListZipperOp a b
f *>> k =
  mkListZipperOp f <* k

infixl 5 *>>

(<<*) :: 
  ListZipperOp a c
  -> (ListZipper a -> Maybe b)
  -> ListZipperOp a b
(<<*) =
  flip (*>>)

infixl 5 <<*

mkListZipperOp' ::
  (ListZipper a -> Maybe (ListZipper a))
  -> ListZipperOp' a
mkListZipperOp' f = 
  ListZipperOp (\s -> (\s' -> (s', ())) <$> f s)

(.>>) ::
  (ListZipper a -> Maybe (ListZipper a))
  -> ListZipperOp a b
  -> ListZipperOp' a
f .>> k =
  mkListZipperOp' f <* k

infixl 5 .>>

(<<.) ::
  ListZipperOp a b
  -> (ListZipper a -> Maybe (ListZipper a))
  -> ListZipperOp' a
(<<.) =
  flip (.>>)

infixl 5 <<.

runListZipperOp ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe (ListZipper a, x)
runListZipperOp (ListZipperOp o) z =
  o z

execListZipperOp ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe (ListZipper a)
execListZipperOp o =
  fmap (^. _1) . runListZipperOp o

(##>) ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe (ListZipper a)
(##>) =
  execListZipperOp

infixl 6 ##>

(<##) ::
  ListZipper a
  -> ListZipperOp a x
  -> Maybe (ListZipper a)
(<##) =
  flip (##>)

infixl 6 <##

evalListZipperOp ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe x
evalListZipperOp o =
  fmap (^. _2) . runListZipperOp o

(&&>) ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe x
(&&>) =
  evalListZipperOp

infixl 6 &&>

(<&&) ::
  ListZipper a
  -> ListZipperOp a x
  -> Maybe x
(<&&) =
  flip (&&>)

infixl 6 <&&

execOpList ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe [a]
execOpList o =
  fmap list . execListZipperOp o

(%%>) ::
  ListZipperOp a x
  -> ListZipper a
  -> Maybe [a]
(%%>) =
  execOpList

infixl 5 %%>

(<%%) ::
  ListZipper a
  -> ListZipperOp a x
  -> Maybe [a]
(<%%) =
  flip (%%>)

infixl 5 <%%

execOpList' ::
  ListZipperOp a x
  -> ListZipper a
  -> [a]
execOpList' o =
  fromMaybe [] . execOpList o

($$>) ::
  ListZipperOp a x
  -> ListZipper a
  -> [a]
($$>) =
  execOpList'

infixl 5 $$>

(<$$) ::
  ListZipper a
  -> ListZipperOp a x
  -> [a]
(<$$) =
  flip ($$>)

infixl 5 <$$

moveLeft ::
  ListZipperOp' a
moveLeft =
  mkListZipperOp' (\z ->
    case z of
      ListZipper [] _ _ ->
        Nothing
      ListZipper (h:t) x r ->
        Just (ListZipper t h (x:r))
    )

moveRight ::
  ListZipperOp' a
moveRight =
  mkListZipperOp' (\z ->
    case z of
      ListZipper _ _ [] ->
        Nothing
      ListZipper l x (h:t) ->
        Just (ListZipper (x:l) h t)
  )

opWith ::
  ListZipperOp a b
  -> (a -> Maybe c)
  -> ListZipperOp a c
opWith o p =
  ListZipperOp (\z ->
    let go z' =
          let x = z' ^. focus
          in  case p x of
                Nothing ->
                  go =<< execListZipperOp o z'
                Just w ->
                  Just (z', w)
    in  go z
  )

moveLeftWith ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveLeftWith =
  opWith moveLeft

moveRightWith ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveRightWith =
  opWith moveRight

moveLeftRightWith ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveLeftRightWith p =
  moveLeftWith p <!> moveRightWith p

moveRightLeftWith ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveRightLeftWith p =
  moveRightWith p <!> moveLeftWith p

opWithThen ::
  ListZipperOp a b
  -> (a -> Maybe c)
  -> ListZipperOp a c
opWithThen o p =
  opWith o p <* o

moveLeftWithThen ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveLeftWithThen =
  opWithThen moveLeft

moveRightWithThen ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveRightWithThen =
  opWithThen moveRight

moveLeftRightWithThen ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveLeftRightWithThen p =
  moveLeftWithThen p <!> moveRightWithThen p

moveRightLeftWithThen ::
  (a -> Maybe c)
  -> ListZipperOp a c
moveRightLeftWithThen p =
  moveRightWithThen p <!> moveLeftWithThen p

opUntil ::
  ListZipperOp a x
  -> (a -> Bool)
  -> ListZipperOp' a
opUntil o p =
  opWith o (\a -> if p a then Just () else Nothing)

moveLeftUntil ::
  (a -> Bool)
  -> ListZipperOp' a
moveLeftUntil =
  opUntil moveLeft

moveRightUntil ::
  (a -> Bool)
  -> ListZipperOp' a
moveRightUntil =
  opUntil moveRight

moveLeftRightUntil ::
  (a -> Bool)
  -> ListZipperOp' a
moveLeftRightUntil p =
  moveLeftUntil p <!> moveRightUntil p

moveRightLeftUntil ::
  (a -> Bool)
  -> ListZipperOp' a
moveRightLeftUntil p =
  moveRightUntil p <!> moveLeftUntil p

opUntilThen ::
  ListZipperOp a x
  -> (a -> Bool)
  -> ListZipperOp' a
opUntilThen o p =
  opUntil o p <* o

moveLeftUntilThen ::
  (a -> Bool)
  -> ListZipperOp' a
moveLeftUntilThen =
  opUntilThen moveLeft

moveRightUntilThen ::
  (a -> Bool)
  -> ListZipperOp' a
moveRightUntilThen =
  opUntilThen moveRight

moveLeftRightUntilThen ::
  (a -> Bool)
  -> ListZipperOp' a
moveLeftRightUntilThen p =
  moveLeftUntilThen p <!> moveRightUntilThen p

moveRightLeftUntilThen ::
  (a -> Bool)
  -> ListZipperOp' a
moveRightLeftUntilThen p =
  moveRightUntilThen p <!> moveLeftUntilThen p

opWhileJust ::
  ListZipperOp' a
  -> ListZipper a
  -> ListZipper a
opWhileJust o z =
  case execListZipperOp o z of
    Nothing ->
      z
    Just z' ->
      opWhileJust o z'

deleteStepLeft ::
  ListZipperOp a a
deleteStepLeft =
  ListZipperOp (\(ListZipper l x r) ->
    case l of
      [] ->
        Nothing
      h:t ->
        Just (ListZipper t h r, x)
  )

deleteStepRight ::
  ListZipperOp a a
deleteStepRight =
  ListZipperOp (\(ListZipper l x r) ->
    case r of
      [] ->
        Nothing
      h:t ->
        Just (ListZipper l h t, x)
  )

modifyFocus ::
  (a -> a)
  -> ListZipperOp a a
modifyFocus f =
  state (\(ListZipper l x r) -> (x, ListZipper l (f x) r))

setFocus ::
  a
  -> ListZipperOp a a
setFocus =
  modifyFocus . pure

deriveEq1 ''ListZipper
deriveShow1 ''ListZipper
