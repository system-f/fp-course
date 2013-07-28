{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Structure.ListZipper where

import Data.List
import Monad.Fuunctor

-- $setup
-- >>> import Data.Maybe(isNothing)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper [a] a [a]
  deriving Eq

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- Exercise 1
-- Relative Difficulty: 2
--
-- | Implement the `Fuunctor` instance for `ListZipper`.
--
-- >>> fmaap (+1) (ListZipper [3,2,1] 4 [5,6,7])
-- [2,3,4]⋙5⋘[6,7,8]
instance Fuunctor ListZipper where
  fmaap f (ListZipper l x r) =
    ListZipper (fmap f l) (f x) (fmap f r)

-- Exercise 2
-- Relative Difficulty: 2
--
-- | Implement the `Fuunctor` instance for `MaybeListZipper`.
--
-- >>> fmaap (+1) (IsZ (ListZipper [3,2,1] 4 [5,6,7]))
-- [2,3,4]⋙5⋘[6,7,8]
instance Fuunctor MaybeListZipper where
  fmaap f (IsZ z) =
    IsZ (fmaap f z)
  fmaap _ IsNotZ =
    IsNotZ

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- prop> xs == toList (fromList xs)
fromList ::
  [a]
  -> MaybeListZipper a
fromList [] =
  IsNotZ
fromList (h:t) =
  IsZ (ListZipper [] h t)

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> null xs == isNothing (toMaybe (fromList xs))
toMaybe ::
  MaybeListZipper a
  -> Maybe (ListZipper a)
toMaybe IsNotZ =
  Nothing
toMaybe (IsZ z) =
  Just z

-- The `ListZipper'` type-class that will permit overloading operations.
class Fuunctor f => ListZipper' f where
  toMaybeListZipper ::
    f a
    -> MaybeListZipper a
  fromListZipper ::
    ListZipper a
    -> f a

instance ListZipper' ListZipper where
  toMaybeListZipper =
    IsZ
  fromListZipper =
    id

instance ListZipper' MaybeListZipper where
  toMaybeListZipper =
    id
  fromListZipper =
    IsZ

-- Exercise 5
-- Relative Difficulty: 2
--
-- | Convert the given zipper back to a list.
toList ::
  ListZipper' f =>
  f a
  -> [a]
toList z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> reverse l ++ x:r
    IsNotZ -> []

-- Exercise 6
-- Relative Difficulty: 3
--
-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (ListZipper [] 0 [1])
-- []⋙1⋘[1]
--
-- >>> withFocus (+1) (ListZipper [1,0] 2 [3,4])
-- [0,1]⋙3⋘[3,4]
withFocus ::
  ListZipper' f =>
  (a -> a)
  -> f a
  -> f a
withFocus f z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> fromListZipper (ListZipper l (f x) r)
    IsNotZ -> z

-- Exercise 7
-- Relative Difficulty: 2
--
-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (ListZipper [] 0 [1])
-- []⋙1⋘[1]
--
-- >>> setFocus 1 (ListZipper [1,0] 2 [3,4])
-- [0,1]⋙1⋘[3,4]
setFocus ::
  ListZipper' f =>
  a
  -> f a
  -> f a
setFocus =
  withFocus . const

-- A flipped infix alias for `setFocus`. This allows:
--
-- z := "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper' f =>
  f a
  -> a
  -> f a
(.=) =
  flip setFocus

-- Exercise 8
-- Relative Difficulty: 2
--
-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (ListZipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (ListZipper [] 0 [1,2])
-- False
hasLeft ::
  ListZipper' f =>
  f a
  -> Bool
hasLeft z =
  case toMaybeListZipper z of
    IsZ (ListZipper l _ _) -> not (null l)
    IsNotZ -> False

-- Exercise 9
-- Relative Difficulty: 2
--
-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (ListZipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (ListZipper [1,0] 2 [])
-- False
hasRight ::
  ListZipper' f =>
  f a
  -> Bool
hasRight z =
  case toMaybeListZipper z of
    IsZ (ListZipper _ _ r) -> not (null r)
    IsNotZ -> False

-- Exercise 10
-- Relative Difficulty: 3
--
-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- prop> findLeft (const True) (fromList xs) == fromList xs
--
-- prop> case xs of [] -> findLeft (const False) IsNotZ == IsNotZ; (x:xs') -> findLeft (const False) (IsZ (ListZipper [] x xs')) == IsNotZ
findLeft ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findLeft p z = case toMaybeListZipper z of
  IsNotZ -> IsNotZ
  IsZ (ListZipper ls x rs) -> case break p (x:ls) of
    (rs', x':ls') -> IsZ (ListZipper ls' x' (reverse rs' ++ rs))
    _ -> IsNotZ

-- Exercise 11
-- Relative Difficulty: 3
--
-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- prop> findRight (const True) (fromList xs) == fromList xs
--
-- prop> case xs of [] -> findRight (const False) IsNotZ == IsNotZ; (x:xs') -> findRight (const False) (IsZ (ListZipper [] x xs')) == IsNotZ
findRight ::
  ListZipper' f =>
  (a -> Bool)
  -> f a
  -> MaybeListZipper a
findRight p z = case toMaybeListZipper z of
  IsNotZ -> IsNotZ
  IsZ (ListZipper ls x rs) ->
    case break p (x:rs) of
      (ls', x':rs') -> IsZ (ListZipper (reverse ls' ++ ls) x' rs')
      _ -> IsNotZ

-- Exercise 12
-- Relative Difficulty: 4
-- | Move the zipper left, or if there are no elements to the left, go to the far right.
-- CAUTION: This function is non-total, why?
--
-- >>> moveLeftLoop (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2]⋙3⋘[4,5,6,7]
--
-- >>> moveLeftLoop (ListZipper [] 1 [2,3,4])
-- [1,2,3]⋙4⋘[]
moveLeftLoop ::
  ListZipper' f =>
  f a
  -> f a
moveLeftLoop z =
  case toMaybeListZipper z of
    IsZ (ListZipper [] x r) -> let (x':r') = (reverse (x:r))
                               in fromListZipper (ListZipper r' x' [])
    IsZ (ListZipper (h:t) x r) -> fromListZipper (ListZipper t h (x:r))
    IsNotZ -> z

-- Exercise 13
-- Relative Difficulty: 4
-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3,4]⋙5⋘[6,7]
--
-- >>> moveRightLoop (ListZipper [3,2,1] 4 [])
-- []⋙1⋘[2,3,4]
moveRightLoop ::
  ListZipper' f =>
  f a
  -> f a
moveRightLoop z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x []) -> let (x':l') = reverse (x:l)
                               in fromListZipper (ListZipper [] x' l')
    IsZ (ListZipper l x (h:t)) -> fromListZipper (ListZipper (x:l) h t)
    IsNotZ -> z


-- Exercise 14
-- Relative Difficulty: 3
-- | Move the zipper one position to the left.
--
-- >>> moveLeft (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2]⋙3⋘[4,5,6,7]
--
-- >>> moveLeft (ListZipper [] 1 [2,3,4])
-- ∅
moveLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveLeft z =
  case toMaybeListZipper z of
    IsZ (ListZipper [] _ _) -> IsNotZ
    IsZ (ListZipper (h:t) x r) -> IsZ (ListZipper t h (x:r))
    IsNotZ -> IsNotZ

-- Exercise 15
-- Relative Difficulty: 3
-- | Move the zipper one position to the right.
--
-- >>> moveRight (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3,4]⋙5⋘[6,7]
--
-- >>> moveRight (ListZipper [3,2,1] 4 [])
-- ∅
moveRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
moveRight z =
  case toMaybeListZipper z of
    IsZ (ListZipper _ _ []) -> IsNotZ
    IsZ (ListZipper l x (h:t)) -> IsZ (ListZipper (x:l) h t)
    IsNotZ -> IsNotZ

-- Exercise 16
-- Relative Difficulty: 3
-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,4]⋙3⋘[5,6,7]
--
-- >>> swapLeft (ListZipper [] 1 [2,3,4])
-- ∅
swapLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapLeft z =
  case toMaybeListZipper z of
    IsZ (ListZipper [] _ _) -> IsNotZ
    IsZ (ListZipper (h:t) x r) -> IsZ (ListZipper (x:t) h r)
    IsNotZ -> IsNotZ

-- Exercise 17
-- Relative Difficulty: 3
-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3]⋙5⋘[4,6,7]
--
-- >>> swapRight (ListZipper [3,2,1] 4 [])
-- ∅
swapRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
swapRight z =
  case toMaybeListZipper z of
    IsZ (ListZipper _ _ []) -> IsNotZ
    IsZ (ListZipper l x (h:t)) -> IsZ (ListZipper l h (x:t))
    IsNotZ -> IsNotZ

-- Exercise 18
-- Relative Difficulty: 3
-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (ListZipper [3,2,1] 4 [5,6,7])
-- []⋙4⋘[5,6,7]
--
-- >>> dropLefts (ListZipper [] 1 [2,3,4])
-- []⋙1⋘[2,3,4]
--
-- prop> dropLefts (ListZipper l x r) == ListZipper [] x r
dropLefts ::
  ListZipper' f =>
  f a
  -> f a
dropLefts z =
  case toMaybeListZipper z of
    IsZ (ListZipper _ x r) -> fromListZipper (ListZipper [] x r)
    IsNotZ -> z

-- Exercise 19
-- Relative Difficulty: 3
-- | Drop all values to the right of the focus.
--
-- >>> dropRights (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3]⋙4⋘[]
--
-- >>> dropRights (ListZipper [3,2,1] 4 [])
-- [1,2,3]⋙4⋘[]
--
-- prop> dropRights (ListZipper l x r) == ListZipper l x []
dropRights ::
  ListZipper' f =>
  f a
  -> f a
dropRights z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x _) -> fromListZipper (ListZipper l x [])
    IsNotZ -> z

-- Exercise 20
-- Relative Difficulty: 4
-- Move the focus left the given number of positions. If the value is negative, move right instead.
moveLeftN ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
moveLeftN n z = case toMaybeListZipper z of
  IsNotZ -> IsNotZ
  IsZ z' | n == 0    -> fromListZipper z'
         | n < 0     -> moveRightN (negate n) z'
         | otherwise -> moveLeftN (pred n) (moveLeft z')

-- Exercise 21
-- Relative Difficulty: 4
-- Move the focus right the given number of positions. If the value is negative, move left instead.
moveRightN ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
moveRightN n z = case toMaybeListZipper z of
  IsNotZ -> IsNotZ
  IsZ z' | n == 0    -> fromListZipper z'
         | n < 0     -> moveLeftN (negate n) z'
         | otherwise -> moveRightN (pred n) (moveRight z')

-- Exercise 22
-- Relative Difficulty: 6
-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (ListZipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1,2]⋙3⋘[4,5,6,7]
--
-- >>> moveLeftN' 0 (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1,2,3]⋙4⋘[5,6,7]
--
-- >>> moveLeftN' (-2) (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1,2,3,4,5]⋙6⋘[7]
--
-- >>> moveLeftN' (-4) (ListZipper [3,2,1] 4 [5,6,7])
-- Left 3
moveLeftN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveLeftN' n z =
  let moveLeftN'' n' z' q =
        if n' == 0
          then
            Right z'
          else
            if n' < 0
              then
                moveRightN' (negate n') z
              else
                case moveLeft z' of
                  IsZ zz -> moveLeftN'' (n' - 1) (fromListZipper zz) (q + 1)
                  IsNotZ -> Left q
  in moveLeftN'' n z 0

-- Exercise 23
-- Relative Difficulty: 6
-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (ListZipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1,2,3,4]⋙5⋘[6,7]
--
-- >>> moveRightN' 0 (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1,2,3]⋙4⋘[5,6,7]
--
-- >>> moveRightN' (-2) (ListZipper [3,2,1] 4 [5,6,7])
-- Right [1]⋙2⋘[3,4,5,6,7]
--
-- >>> moveRightN' (-4) (ListZipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' ::
  ListZipper' f =>
  Int
  -> f a
  -> Either Int (f a)
moveRightN' n z =
  let moveRightN'' n' z' q =
        if n' == 0
          then
            Right z'
          else
            if n' < 0
              then
                moveLeftN' (negate n') z
              else
                case moveRight z' of
                  IsZ zz -> moveRightN'' (n' - 1) (fromListZipper zz) (q + 1)
                  IsNotZ -> Left q
  in moveRightN'' n z 0

-- Exercise 24
-- Relative Difficulty: 7
-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (ListZipper [3,2,1] 4 [5,6,7])
-- [1]⋙2⋘[3,4,5,6,7]
--
-- >>> nth 5 (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3,4,5]⋙6⋘[7]
--
-- >>> nth 8 (ListZipper [3,2,1] 4 [5,6,7])
-- ∅
nth ::
  ListZipper' f =>
  Int
  -> f a
  -> MaybeListZipper a
nth i z =
  if i < 0
    then
      IsNotZ
    else
      case toMaybeListZipper z of
        g@(IsZ z') -> case moveLeftN' i z' of
                                 Left a -> moveRightN (i-a) z
                                 Right (ListZipper l _ _) -> moveLeftN (length l) g
        z'@IsNotZ -> z'

-- Exercise 25
-- Relative Difficulty: 4
-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (ListZipper [3,2,1] 4 [5,6,7])
-- Just 3
--
-- prop> maybe True (\i -> maybe False (==z) (toMaybe (nth i z))) (index z)
index ::
  ListZipper' f =>
  f a
  -> Maybe Int
index z =
  case toMaybeListZipper z of
    IsZ (ListZipper l _ _) -> Just (length l)
    IsNotZ -> Nothing

-- Exercise 26
-- Relative Difficulty: 5
-- | Move the focus to the end of the zipper.
-- CAUTION: This function is non-total, why?
--
-- >>> end (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3,4,5,6]⋙7⋘[]
end ::
  ListZipper' f =>
  f a
  -> f a
end z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> let (x':r') = reverse (x:r)
                                       in fromListZipper (ListZipper (r' ++ l) x' [])
    IsNotZ -> z

-- Exercise 27
-- Relative Difficulty: 5
-- | Move the focus to the start of the zipper.
--
-- >>> start (ListZipper [3,2,1] 4 [5,6,7])
-- []⋙1⋘[2,3,4,5,6,7]
start ::
  ListZipper' f =>
  f a
  -> f a
start z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> let (x':r') = reverse (x:l)
                                       in fromListZipper (ListZipper [] x' (r' ++ r))
    IsNotZ -> z

-- Exercise 28
-- Relative Difficulty: 5
-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2]⋙3⋘[5,6,7]
--
-- >>> deletePullLeft (ListZipper [] 1 [2,3,4])
-- ∅
deletePullLeft ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullLeft z =
  case toMaybeListZipper z of
    IsZ (ListZipper [] _ _) -> IsNotZ
    IsZ (ListZipper (h:t) _ r) -> IsZ (ListZipper t h r)
    IsNotZ -> IsNotZ

-- Exercise 29
-- Relative Difficulty: 5
-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3]⋙5⋘[6,7]
--
-- >>> deletePullRight (ListZipper [3,2,1] 4 [])
-- ∅
deletePullRight ::
  ListZipper' f =>
  f a
  -> MaybeListZipper a
deletePullRight z =
  case toMaybeListZipper z of
    IsZ (ListZipper _ _ []) -> IsNotZ
    IsZ (ListZipper l _ (h:t)) -> IsZ (ListZipper l h t)
    IsNotZ -> IsNotZ

-- Exercise 30
-- Relative Difficulty: 5
-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3,4]⋙15⋘[5,6,7]
--
-- >>> insertPushLeft 15 (ListZipper [] 1 [2,3,4])
-- [1]⋙15⋘[2,3,4]
--
-- prop> maybe False (==z) (toMaybe (deletePullLeft (insertPushLeft i z)))
insertPushLeft ::
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushLeft a z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> fromListZipper (ListZipper (x:l) a r)
    IsNotZ -> z

-- Exercise 31
-- Relative Difficulty: 5
-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (ListZipper [3,2,1] 4 [5,6,7])
-- [1,2,3]⋙15⋘[4,5,6,7]
--
-- >>> insertPushRight 15 (ListZipper [3,2,1] 4 [])
-- [1,2,3]⋙15⋘[4]
--
-- prop> maybe False (==z) (toMaybe (deletePullRight (insertPushRight i z)))
insertPushRight ::
  ListZipper' f =>
  a
  -> f a
  -> f a
insertPushRight a z =
  case toMaybeListZipper z of
    IsZ (ListZipper l x r) -> fromListZipper (ListZipper l a (x:r))
    IsNotZ -> z

-- Let's start using proper type-class names.
--
-- The following type-class hierarchy does not correspond to the GHC base library hierarchy.
-- However, it is much more flexible, which we exploit here.

class Fuunctor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

class Apply f => Applicative f where
  unit ::
    a -> f a

class Fuunctor f => Extend f where
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

class Extend f => Comonad f where
  counit ::
    f a
    -> a

class Fuunctor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

-- The `Traversable` instance for `[]` is implemented for demonstration.
-- It will also come in use later.
instance Traversable [] where
  traverse f =
    foldr (\a b -> fmaap (:) (f a) <*> b) (unit [])

-- Exercise 31
-- Relative Difficulty: 6
-- | Implement the `Apply` instance for `ListZipper`.
-- This implementation zips functions with values by function application.
--
-- >>> ListZipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> ListZipper [3,2,1] 4 [5,6,7]
-- [12,5]⋙8⋘[15,24,12]
instance Apply ListZipper where
  ListZipper fl fx fr <*> ListZipper al ax ar =
    ListZipper (zipWith ($) fl al) (fx ax) (zipWith ($) fr ar)

-- Exercise 32
-- Relative Difficulty: 4
-- | Implement the `Apply` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `<*>` for `ListZipper`.
instance Apply MaybeListZipper where
  IsNotZ <*> _ = IsNotZ
  _ <*> IsNotZ = IsNotZ
  IsZ f <*> IsZ a = IsZ (f <*> a)

-- Exercise 33
-- Relative Difficulty: 5
-- | Implement the `Applicative` instance for `ListZipper`.
-- This implementation produces an infinite list zipper (to both left and right).
--
-- /Tip:/ Use @Data.List#repeat@.
instance Applicative ListZipper where
  unit a =
    ListZipper (repeat a) a (repeat a)

-- Exercise 34
-- Relative Difficulty: 4
-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @unit@ for `ListZipper`.
instance Applicative MaybeListZipper where
  unit =
    IsZ . unit

-- Exercise 35
-- Relative Difficulty: 7
-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @Data.List#unfoldr@.
--
-- >>> id <<= (ListZipper [2,1] 3 [4,5])
-- [[]⋙1⋘[2,3,4,5],[1]⋙2⋘[3,4,5]]⋙[1,2]⋙3⋘[4,5]⋘[[1,2,3]⋙4⋘[5],[1,2,3,4]⋙5⋘[]]
instance Extend ListZipper where
  f <<= z =
    ListZipper (unfoldr (fmap (\z' -> (f z', z')) . toMaybe . moveLeft) z) (f z) (unfoldr (fmap (\z' -> (f z', z')) . toMaybe . moveRight) z)

-- Exercise 36
-- Relative Difficulty: 3
-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> counit (ListZipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  counit (ListZipper _ x _) =
    x

-- Exercise 37
-- Relative Difficulty: 10
-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
instance Traversable ListZipper where
  traverse f (ListZipper l x r) =
    fmaap (ListZipper . reverse) (traverse f $ reverse l) <*> f x <*> traverse f r

-- Exercise 38
-- Relative Difficulty: 5
-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
instance Traversable MaybeListZipper where
  traverse _ IsNotZ =
    unit IsNotZ
  traverse f (IsZ z) =
    fmaap IsZ (traverse f z)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    (show . reverse $ l) ++ ('⋙':show x ++ "⋘") ++ show r

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "∅"