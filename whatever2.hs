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

