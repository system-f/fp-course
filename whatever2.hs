data Two a = Two a a deriving (Eq, Show)
data Optional a = Empty | Full a deriving (Eq, Show)
data Inter a = Inter (Int -> a)

runInter :: Inter a -> Int -> a
runInter (Inter v) = v

mapTwo :: (a -> b) -> Two a -> Two b
mapTwo f (Two a1 a2) = Two (f a1) (f a2)

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

mapInter :: (a -> b) -> Inter a -> Inter b
mapInter f (Inter g) = Inter (f . g)

-- DON'T REPEAT YOURSELF YOURSELF
flipTwo :: Two (a -> b) -> a -> Two b
flipTwo two a = mapTwo (\f -> f a) two

flipOptional :: Optional (a -> b) -> a -> Optional b
flipOptional opt a = mapOptional (\f -> f a) opt

flipInter :: Inter (a -> b) -> a -> Inter b
flipInter ir a = mapInter (\f -> f a) ir

flipOptionalTwo :: Optional (Two (a -> b)) -> a -> Optional (Two b)
flipOptionalTwo opttwo a = mapOptional (mapTwo (\f -> f a)) opttwo

-- (covariant) Functor
class AllThingsThatMap k where
  mapAnything :: (a -> b) -> k a -> k b

-- law1, law of identity, mapAnything id x = x
-- law2, law of composition, mapAnything f (mapAnything g x) = 
--   mapAnything (f . g) x

instance AllThingsThatMap Two where
  mapAnything = mapTwo

instance AllThingsThatMap Optional where
  mapAnything = mapOptional
flipAnything :: AllThingsThatMap k => k (a -> b) -> a -> k b
flipAnything x a = mapAnything (\f -> f a) x

data ListAnd x a = ListAnd [x] a deriving (Eq, Show)

instance AllThingsThatMap (ListAnd x) where
  -- (a -> b) -> ListAnd x a -> ListAnd x b
  mapAnything f (ListAnd x a) =
    ListAnd x (f a)

bindOptional :: Optional a -> (a -> Optional b) -> Optional b
bindOptional Empty _ = Empty
bindOptional (Full a) f = f a

pureOptional :: a -> Optional a
pureOptional a = Full a

bindInter :: Inter a -> (a -> Inter b) -> Inter b
bindInter (Inter g) f = Inter (\a -> case f (g a) of Inter j -> j a)

pureInter :: a -> Inter a
pureInter a = Inter (\_ -> a)

bindListAnd :: ListAnd x a -> (a -> ListAnd x b) -> ListAnd x b
bindListAnd (ListAnd x a) f = case f a of ListAnd y b -> ListAnd (x ++ y) b

pureListAnd :: a -> ListAnd x a
pureListAnd a = ListAnd [] a

----
-- DRY no really

sequenceOptional :: [Optional a] -> Optional [a]
sequenceOptional =
  foldr
    (\h t ->
      bindOptional h (\a ->
      bindOptional t (\b ->
      pureOptional (a:b))))
    (pureOptional [])

sequenceInter :: [Inter a] -> Inter [a]
sequenceInter =
  foldr
    (\h t ->
      bindInter h (\a ->
      bindInter t (\b ->
      pureInter (a:b))))
    (pureInter [])

sequenceListAnd :: [ListAnd x a] -> ListAnd x [a]
sequenceListAnd =
  foldr
    (\h t ->
      bindListAnd h (\a ->
      bindListAnd t (\b ->
      pureListAnd (a:b))))
    (pureListAnd [])

-- Monad
class AllThingsThatHaveBindAndPure k where
  bind :: k a -> (a -> k b) -> k b
  puure :: a -> k a

-- aka Applicative
class AllThingsThatHaveApplyAndPure k where
  apply :: k (a -> b) -> k a -> k b
  lift0 :: a -> k a

instance AllThingsThatHaveApplyAndPure Optional where
  apply Empty _ = Empty
  apply _ Empty = Empty
  apply (Full f) (Full a) = Full (f a)
  lift0 a = Full a

instance AllThingsThatHaveApplyAndPure Inter where
  apply (Inter f) (Inter x) =
    Inter (\a -> (f a) (x a))
  lift0 a = Inter (\_ -> a)

instance AllThingsThatHaveApplyAndPure [] where
  lift0 a = [a]
  [] `apply` _ = []
  _ `apply` [] = []
  (h1:t1) `apply` r =
    map h1 r ++ (t1 `apply` r)

  -- apply :: Optional (a -> b) -> Optional a -> Optional b
  -- lift0 :: a -> Optional a

-- all things that have apply and lift0, have map
-- all applicative, are functor
lift1 :: AllThingsThatHaveApplyAndPure k => (a -> b) -> (k a -> k b)
-- lift1 = \a2b -> \x -> apply (lift0 a2b) x
-- lift1 = \a2b -> apply (lift0 a2b)
lift1 = apply . lift0

sequenceAgain :: AllThingsThatHaveApplyAndPure k => [k a] -> k [a]
sequenceAgain = foldr (lift2 (:)) (lift0 [])

lift2 :: AllThingsThatHaveApplyAndPure k => (a -> b -> c) -> (k a -> k b -> k c)
lift2 a2b2c ka kb = apply (lift1 a2b2c ka) kb
-- k (b -> c)
-- a2b2c :: a -> b -> c
-- ka :: k a
-- kb :: k b
-- _  :: k c


-- you can write liftN using lift[N-1] and apply

-- a2b :: a -> b
-- _ :: k (a -> b)


-- comonad
class AllThingsThatHaveCobindAndCopure k where
  cobind :: (k a -> b) -> k a -> k b
  copure :: k a -> a

-- (a -> b) -> k a -> k b
mapIt :: AllThingsThatHaveBindAndPure k => (a -> b) -> k a -> k b
mapIt f k = bind k (puure . f)

instance AllThingsThatHaveBindAndPure IO where
  bind = (>>=)
  puure = pure

instance AllThingsThatHaveBindAndPure Optional where
  bind = bindOptional
  puure = Full

instance AllThingsThatHaveBindAndPure Inter where
  bind = bindInter
  puure = pureInter

instance AllThingsThatHaveBindAndPure (ListAnd x) where
  bind = bindListAnd
  puure = pureListAnd

sequenceAnything :: AllThingsThatHaveBindAndPure k => [k a] -> k [a]
sequenceAnything =
  foldr (\h t ->
    bind h (\a ->
    bind t (\b ->
    puure (a:b))))
    (puure [])
-- [k a] -> k [a]

--   bind :: k a -> (a -> k b) -> k b
veryImportantProgram =
  bind (readFile "/etc/passwd") (\s ->
  bind (readFile "/etc/group") (\t ->
  pure (reverse t ++ reverse s)))

