module Network.Server.Common.Lens where

-- | A lens is a pair of set and get.
--
-- The type parameter 'a' denotes the target object.
-- The type parameter 'b' denotes the field object.
data Lens a b =
  Lens (a -> b -> a) (a -> b)

-- | Given a lens and a target object, return its field object.
getL ::
  Lens a b
  -> a
  -> b
getL (Lens _ g) =
  g

-- | Given a lens, a target object and a field object, return a new target object with the field set.
setL ::
  Lens a b
  -> a
  -> b
  -> a
setL (Lens s _) =
  s

-- | Produce the lens for the first element of a pair.
--
-- >>> getL fstL ("hi", 3)
-- "hi"
--
-- >>> setL fstL ("hi", 3) "bye"
-- ("bye",3)
fstL ::
  Lens (a, b) a
fstL =
  Lens (\(_, b) a -> (a, b)) fst

-- | Produce the lens for the second element of a pair.
--
-- >>> getL sndL ("hi", 3)
-- 3
--
-- >>> setL sndL ("hi", 3) 4
-- ("hi",4)
sndL ::
  Lens (a, b) b
sndL =
  Lens (\(a, _) b -> (a, b)) snd

-- | Lens composition.
-- Given lens (a to b) and lens (b to c), produce lens (a to c).
--
-- >>> getL (fstL .@ sndL) (("hi", 3), [7,8,9])
-- 3
--
-- >>> setL (fstL .@ sndL) (("hi", 3), [7,8,9]) 4
-- (("hi",4),[7,8,9])
(.@) ::
  Lens a b
  -> Lens b c
  -> Lens a c
Lens s1 g1 .@ Lens s2 g2 =
  Lens (\a -> s1 a . s2 (g1 a)) (g2 . g1)

-- | Lens identity.
-- Produce lens that /does nothing/.
--
-- prop> getL identityL (x :: Int) == x
--
-- prop> setL identityL x (y :: Int) == y
identityL ::
  Lens a a
identityL =
  Lens (const id) id

-- | Lens modification.
-- Given a lens and a modification function on the field object
-- and a target object, return a target with the function applied at that field.
--
-- >>> modify fstL (+10) (4, "hi")
-- (14,"hi")
modify ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
modify (Lens s g) f a =
  s a (f (g a))

iso ::
  (a -> b)
  -> (b -> a)
  -> Lens a b
iso f g =
  Lens (const g) f
