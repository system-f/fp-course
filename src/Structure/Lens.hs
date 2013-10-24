{-# LANGUAGE NoImplicitPrelude #-}

module Structure.Lens where

import Core
import Data.List
import Monad.State
import Monad.Functor
import Structure.ListZipper

-- $setup
-- >>> import Core(Num(..), reverse, fst, snd)

data Address =
  Address
    String -- street
    String -- suburb
  deriving Eq

-- an employee has an address
data Employee =
  Employee
    String -- name
    Int -- age
    Address
  deriving Eq

-- a company has an address, a CEO (employee) and a list of employees
data Company =
  Company
    String -- name
    Address
    Employee -- CEO
    [Employee] -- employees
  deriving Eq

-- | An example of a company value.
--
-- >>> company
-- ACME located at Acme St, Acmeville with CEO Bob aged 13 from Bob St, Bobville and employees; [Mary aged 14 from Mary St, Maryville,Fred aged 15 from Fred St, Fredville]
company ::
  Company
company =
  Company
    "ACME"
    (Address
      "Acme St"
      "Acmeville")
    (Employee
      "Bob"
      13
      (Address
        "Bob St"
        "Bobville"))
    [
      (Employee
        "Mary"
        14
        (Address
          "Mary St"
          "Maryville"))
    , (Employee
          "Fred"
          15
          (Address
            "Fred St"
            "Fredville"))
    ]

instance Show Address where
  show (Address street suburb) =
    concat [street, ", ", suburb]

instance Show Employee where
  show (Employee name age address) =
    intercalate " " [name, "aged", show age, "from", show address]

instance Show Company where
  show (Company name address ceo employees) =
    intercalate " " [name, "located at", show address, "with CEO", show ceo, "and employees;", show employees]

-- Problem
-- Modify the suburb of the address of the employees of a company
updateSuburbs1 ::
  (String -> String)
  -> Company
  -> Company
updateSuburbs1 f (Company name address ceo employees) =
  Company name address ceo (map (\(Employee ename age (Address street suburb)) -> Employee ename age (Address street (f suburb))) employees)
  --                                                                                                                  ^ application
  -- ick!

-- | A lens is a pair of set and get.
--
-- The type parameter 'a' denotes the target object.
-- The type parameter 'b' denotes the field object.
data Lens a b =
  Lens (a -> b -> a) (a -> b)

-- A lens on the address field of a suburb target.
suburbAddress ::
  Lens Address String
suburbAddress =
  Lens
    (\(Address street _) suburb -> Address street suburb)
    (\(Address _ suburb) -> suburb)

-- A lens on the employee field of an address target.
employeeAddress ::
  Lens Employee Address
employeeAddress =
  Lens
    (\(Employee name age _) address -> Employee name age address)
    (\(Employee _ _ address) -> address)

-- A lens on the employees field of a company target.
companyEmployees ::
  Lens Company [Employee]
companyEmployees =
  Lens
    (\(Company name address ceo _) employees -> Company name address ceo employees)
    (\(Company _ _ _ employees) -> employees)

-- A lens on the name field of a company target.
companyName ::
  Lens Company String
companyName =
  Lens
    (\(Company _ address ceo employees) name -> Company name address ceo employees)
    (\(Company name _ _ _) -> name)

-- Exercise 1
--
-- | Given a lens and a target object, return its field object.
--
-- >>> getL companyName company
-- "ACME"
getL ::
  Lens a b
  -> a
  -> b
getL (Lens _ g) =
  g

-- Exercise 2
--
-- | Given a lens, a target object and a field object, return a new target object with the field set.
--
-- >>> setL companyName company "Mickey"
-- Mickey located at Acme St, Acmeville with CEO Bob aged 13 from Bob St, Bobville and employees; [Mary aged 14 from Mary St, Maryville,Fred aged 15 from Fred St, Fredville]
setL ::
  Lens a b
  -> a
  -> b
  -> a
setL (Lens s _) =
  s

-- Exercise 3
--
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

-- Exercise 4
--
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

-- Exercise 5
--
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

-- Exercise 6
--
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

-- Exercise 7
--
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

-- Exercise 8
--
-- | Lens modification in a functor.
-- Given two lenses, one with a functor-like field object,
-- run the given modification function on the given target object.
--
-- >>> (fstL ..@ sndL) (+10) ([("hi", 3)], 44)
-- ([("hi",13)],44)
(..@) ::
  Functor f =>
  Lens a (f a1)
  -> Lens a1 b
  -> (b -> b)
  -> a
  -> a
l1 ..@ l2 =
  modify l1 . fmap . modify l2

-- Exercise 9
--
-- | Given an isomorphism, produce a lens.
--
-- >>> getL (iso reverse reverse) [1,2,3]
-- [3,2,1]
--
-- >>> setL (iso reverse reverse) [1,2,3] [4,5,6]
-- [6,5,4]
iso ::
  (a -> b)
  -> (b -> a)
  -> Lens a b
iso f g =
  Lens (const g) f

-- Exercise 10
--
-- | Given two lenses, produce a lens that switches on Either.
--
-- >>> getL (fstL |.| sndL) (Left ("hi", 3))
-- "hi"
--
-- >>> getL (fstL |.| sndL) (Right ("hi", 3))
-- 3
--
-- >>> setL (fstL |.| sndL) (Left ("hi", 3)) "bye"
-- Left ("bye",3)
--
-- >>> setL (fstL |.| sndL) (Right ("hi", 3)) 4
-- Right ("hi",4)
(|.|) ::
  Lens a c
  -> Lens b c
  -> Lens (Either a b) c
Lens s1 g1 |.| Lens s2 g2 =
  Lens (\e c -> either (\a -> Left (s1 a c)) (\b -> Right (s2 b c)) e) (either g1 g2)

-- Exercise 11
--
-- | Given two lenses, produce a lens that combines on their product.
--
-- >>> getL (fstL *.* sndL) (("hi", 3), ("bye", 4))
-- ("hi",4)
--
-- >>> setL (fstL *.* sndL) (("hi", 3), ("bye", 4)) ("thigh", 5)
-- (("thigh",3),("bye",5))
(*.*) ::
  Lens a b
  -> Lens c d
  -> Lens (a, c) (b, d)
Lens s1 g1 *.* Lens s2 g2 =
  Lens (\(a, c) (b, d) -> (s1 a b, s2 c d)) (\(a, c) -> (g1 a, g2 c))

-- Exercise 12
--
-- | Given a lens, produce a state object.
--
-- >>> runState (stateL fstL) ("hi", 3)
-- ("hi",("hi",3))
--
-- >>> runState (stateL sndL) ("hi", 3)
-- (3,("hi",3))
stateL ::
  Lens a b
  -> State a b
stateL (Lens _ g) =
  State (\a -> (g a, a))

-- Exercise 13
--
-- | Modify the suburb of a company.
--
-- /Tip:/ Use companyEmployees, employeeAddress, suburbAddress.
updateSuburbs2 ::
  (String -> String)
  -> Company
  -> Company
updateSuburbs2 =
  companyEmployees ..@ employeeAddress .@ suburbAddress

-- | A store is the pair of a function from field to target and a field.
data Store a b =
  Store (a -> b) a

strPos ::
  Store a b
  -> a
strPos (Store _ g) =
  g

strPut ::
  Store a b
  -> a
  -> b
strPut (Store s _) =
  s

-- Exercise 14
-- | Store is a functor.
--
-- >>> strPut (fmap (+10) (Store (*2) 3)) 5
-- 20
--
-- prop> strPos (fmap (+10) (Store (*2) x)) == (x :: Int)
instance Functor (Store a) where
  fmap f (Store s g) =
    Store (f . s) g

-- Exercise 15
-- | Store duplicates.
instance Extend (Store a) where
  f <<= Store s g =
    Store (\k -> f (Store s k)) g

-- Exercise 16
-- | Store is a comonad.
instance Comonad (Store a) where
  counit (Store s g) =
    s g

-- | An alternative representation of a lens.
--
-- A function that takes a target object to a store,
-- which is a pair of values:
--
-- * field -> target
--
-- * field
data SLens target field =
  SLens (target -> Store field target)

-- | The lens for the first element of a pair.
sfstL ::
  SLens (a, b) a
sfstL =
  SLens (\(a, b) -> Store (\a' -> (a', b)) a)

-- | The lens for the first element of a pair.
ssndL ::
  SLens (a, b) b
ssndL =
  SLens (\(a, b) -> Store (\b' -> (a, b')) b)

-- Exercise 17
-- | Write the get function for the alternative lens.
--
-- >>> sgetL sfstL ("hi", 3)
-- "hi"
sgetL ::
  SLens a b
  -> a
  -> b
sgetL (SLens q) a =
  let Store _ g = q a in g

-- Exercise 18
-- | Write the set function for the alternative lens.
--
-- >>> ssetL sfstL ("hi", 3) "bye"
-- ("bye",3)
ssetL ::
  SLens a b
  -> a
  -> b
  -> a
ssetL (SLens q) a =
  let Store s _ = q a in s

-- Exercise 19
-- | Write the isomorphism between the two lens structures.
--
-- prop> getL fstL x == getL (snd equivalent sfstL) (x :: (Int, String))
--
-- prop> setL sndL x y == setL (snd equivalent ssndL) (x :: (Int, String)) y
--
-- prop> sgetL sfstL x == sgetL (fst equivalent fstL) x
--
-- prop> ssetL ssndL x y == ssetL (fst equivalent sndL) (x :: (Int, String)) y
equivalent ::
  (
    Lens a b -> SLens a b
  , SLens a b -> Lens a b
  )
equivalent =
  (
    \(Lens s g) -> SLens (\a -> Store (s a) (g a))
  , \(SLens q) -> Lens (\a -> let Store s _ = q a in s) (\a -> let Store _ g = q a in g)
  )

infixr 1 ..@

infixr 2 .@
