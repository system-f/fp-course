{-# LANGUAGE NoImplicitPrelude #-}

module Course.Optional where

import Control.Applicative qualified  as A
import Control.Monad qualified  as M
import Course.Core
import Prelude (String)
--              ^^^^^^ `String` is now in top-level global scope
--     ^^^^^^^ import `Prelude` once, unqualified
import Prelude qualified as P
--                          ^ everything in `Prelude` is now in scope, in the namespace `P`.
--     ^^^^^^^^^^^^^^^^^ import `Prelude` again, qualified this time

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = Empty | Full a
--                             ^ one argument, type `a`
--                        ^^^^ second data constructor
--                ^^^^^ first data constructor. zero arguments.
--            ^ type variable
--   ^^^^^^^^ type name
  deriving (Eq, Show)
  --            ^^^^ compiler generates instance `Show a => Show (Optional a)`
  --        ^^ compiler generates instance `Eq a => Eq (Optional a)`

contains :: Eq a => a -> Optional a -> Bool
--                  ^ constrained type variable
--          ^^^^ constraint: `Eq a` must be true
--               this ensures that `(==)` is defined for signature `a -> a -> Bool`.
contains _ Empty = False
contains a (Full z) = a == z
--                    ^^^^^^ compare the data we found in the second argument to the first argument
--               ^ data carried by the `Full` constructor. `z :: a`
--       ^ first argument. `a :: a`

-- | Question Optional 1
--
-- The `Optional` type we are defining here is analogous to Haskell's std lib `Maybe` type.
-- Find `Maybe` on Hoogle, and navigate to its source.
-- (WARNING: Hoogle search is CasE SEnsItiVe!)
--     a) What are the data constructors of `Maybe`? What are their types?
--     b) What instances does `Maybe` derive? In your answer, state the instance context and the instance head.
question_Optional_1 :: (String, String)
question_Optional_1 = error "todo"

-- | Return the possible value if it exists; otherwise, the first argument.
--
-- >>> fullOr 99 (Full 8)
-- 8
--
-- >>> fullOr 99 Empty
-- 99
fullOr :: a -> Optional a -> a
fullOr = error "todo: Course.Optional#fullOr"

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional = error "todo: Course.Optional#mapOptional"

-- | Question Optional 2
--
-- Look back at `src/Course/ExactlyOne.hs` and `src/Course/Validation.hs`.
-- Look back at `mapExactlyOne` and `mapValidation`.
--     a) Is there a pattern that all of their type signatures fall into?
--     b) Are there similarities in their implementations? Explain.
question_Optional_2 :: (String, String)
question_Optional_2 = error "todo"

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
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional = error "todo: Course.Optional#bindOptional"

-- | Question Optional 3
--
-- Look back at `src/Course/ExactlyOne.hs` and `src/Course/Validation.hs`.
-- Look back at `bindExactlyOne` and `bindValidation`.
--     a) Is there a pattern that all of their type signatures fall into?
--     b) Are there similarities in their implementations? Explain.
question_Optional_3 :: (String, String)
question_Optional_3 = error "todo"

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
(<+>) :: Optional a -> Optional a -> Optional a
(<+>) = error "todo: Course.Optional#(<+>)"

-- | Apply the callback or else use the fallback.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional :: (a -> b) -> b -> Optional a -> b
--                      ^ fallback
--          ^^^^^^^^ callback
optional = error "todo: Course.Optional#optional"

-- | Question Optional 4
--
-- Why does the fallback in `optional` have type `b` rather than type `a`?
question_Optional_4 :: String
question_Optional_4 = error "todo"

-- | Question Optional 5
--
-- Hoogle `maybe`. Read its documentation and navigate to its source.
--     a) How are `maybe` and `optional` similar?
--     b) How are `maybe` and `optional` different?
question_Optional_5 :: (String, String)
question_Optional_5 = error "todo"

-- $noteToTrainee
-- We're done here! Move on to `[Course.List](../List.hs)`

instance P.Functor Optional where
    fmap = M.liftM

instance A.Applicative Optional where
    (<*>) = M.ap
    pure =
        Full

instance P.Monad Optional where
    (>>=) = flip bindOptional

-- $> test test_Optional
-- We're done here! Move on to [Course.List](../List.hs)
