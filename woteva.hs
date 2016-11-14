data Optional a = Full a | Empty
  deriving (Eq, Show)

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

data StringReader a = StringReader (String -> a)

append :: List a -> List a -> List a
append Nil y = y
append (Cons h t) y = Cons h (t `append` y)

pointOptional :: a -> Optional a
pointOptional a = Full a

bindOptional :: Optional a -> (a -> Optional b) -> Optional b
bindOptional Empty _ = Empty
bindOptional (Full a) f = f a

pointList :: a -> List a
pointList a = Cons a Nil

bindList :: List a -> (a -> List b) -> List b
bindList Nil _ = Nil
bindList (Cons h t) f = f h `append` bindList t f

pointStringReader :: a -> StringReader a
pointStringReader a = StringReader (\_ -> a)
-- pointStringReader = StringReader . const

bindStringReader :: StringReader a -> (a -> StringReader b) -> StringReader b
bindStringReader (StringReader f) g =
  StringReader (\s -> let StringReader h = g (f s) in h s)

-- if both are Full, add them
add2Optional :: Optional Int -> Optional Int -> Optional Int
-- Optional a -> (a -> Optional b) -> Optional b
add2Optional a b =
  bindOptional a (\aa ->
  bindOptional b (\bb ->
  pointOptional (aa + bb)))

-- add every Int in a with every Int in b
add2List :: List Int -> List Int -> List Int
-- bindList :: List a -> (a -> List b) -> List b
add2List a b =
  bindList a (\aa -> 
  bindList b (\bb ->
  pointList (aa + bb)))

add2StringReader :: StringReader Int -> StringReader Int -> StringReader Int
-- bindStringReader :: StringReader a -> (a -> StringReader b) -> StringReader b
add2StringReader a b =
  bindStringReader a (\aa ->
  bindStringReader b (\bb ->
  pointStringReader (aa + bb)))

class Moon k where
  -- flatMap, (>>=)
  bind :: k a -> (a -> k b) -> k b
  -- unit, pure, return, point
  point :: a -> k a

-- add2, instances (Optional, List, StringReader)

instance Moon Optional where
  bind = bindOptional
  point = pointOptional

instance Moon List where
  bind = bindList
  point = pointList

instance Moon StringReader where
  bind = bindStringReader
  point = pointStringReader

add2 :: Moon k => k Int -> k Int -> k Int
add2 a b =
  bind a (\aa ->
  bind b (\bb ->
  point (aa + bb)))

{-
  aa <- a
  bb <- b
  return (aa + bb)
-}
