-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

-- TOTAL marks:    /66

module Structure.List where

import Control.Applicative
import Intro.Optional

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :. List t deriving Eq

-- Right-associative
infixr 5 :.

instance (Show t) => Show (List t) where
  show = show . foldRight (:) []

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
--
-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> x `headOr` Nil == x
headOr ::
  a
  -> List a
  -> a
headOr =
  error "todo"

-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
--
-- | Sum the elements of the list.
--
-- >>> suum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- prop> foldLeft (-) (suum x) x == 0
suum ::
  List Int
  -> Int
suum =
  error "todo"

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
--
-- | Return the length of the list.
--
-- >>> len (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> suum (maap (const 1) x) == len x
len ::
  List a
  -> Int
len =
  error "todo"

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
--
-- | Map the given function on each element of the list.
--
-- >>> maap (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> maap id x == x
maap ::
  (a -> b)
  -> List a
  -> List b
maap =
  error "todo"

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
--
-- | Return elements satisfying the given predicate.
--
-- >>> fiilter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> fiilter (const True) x == x
--
-- prop> fiilter (const False) x == Nil
fiilter ::
  (a -> Bool)
  -> List a
  -> List a
fiilter =
  error "todo"

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
--
-- | Append two lists to a new list.
--
-- >>> append (1 :. 2 :. 3 :. Nil) (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> (x `append` y) `append` z == x `append` (y `append` z)
--
-- prop> append x Nil == x
append ::
  List a
  -> List a
  -> List a
append =
  error "todo"

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
--
-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> suum (maap len x) == len (flatten x)
flatten ::
  List (List a)
  -> List a
flatten =
  error "todo"

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
--
-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> flatMap id (x :: List (List Int)) == flatten x
flatMap ::
  (a -> List b)
  -> List a
  -> List b
flatMap =
  error "todo"

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 2.0 marks
-- Elegance: 3.5 marks
-- Total: 9
--
-- | Convert a list of optional values to an optional list of values
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  error "todo"

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
--
-- | Reverse a list.
--
-- >>> rev (1 :. 2 :. 3 :. Nil)
-- [3,2,1]
--
-- prop> (rev . rev) x == x
rev ::
  List a
  -> List a
rev =
  error "todo"

-- END Exercises
