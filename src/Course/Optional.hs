{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional =
  \f -> \a -> case a of
    Empty -> Empty
    Full x -> Full (f x)

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional =
  \f -> \opt_a ->
    case opt_a of
      Empty -> Empty
      Full a -> f a

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) ::
     Optional a -> a -> a
(??) Empty         x =  x
(??) (Full a)      _ =  a

{-
(???) = \o x -> case o of
  Empty -> x
  Full a -> a
-}

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
      Optional a -> Optional a -> Optional a
(<+>) Empty         o           = o
(<+>) r@(Full _)      _           = r

{-

if(x == null) {
  return y
else
  return x

-}


-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional ::
  (a -> b) -> b -> Optional a -> b
optional =
  \f ->      \b -> \opt       ->
    case opt of
      Empty -> 
        b
      Full a ->
        f a

bindOptionalAgain ::
  (a -> Optional b) -> Optional a -> Optional b
bindOptionalAgain =
  \f opt ->
    optional f Empty opt

{-
bindOptional ::
  (a -> Optional b) -> Optional a -> Optional b
bindOptional =
  \f -> \opt_a ->
    case opt_a of
      Empty -> Empty
      Full a -> f a
-}

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
