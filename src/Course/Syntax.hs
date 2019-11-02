module Course.Syntax where

import Prelude
import Data.String

x :: String
x = "Hello"

y :: Int
y = 2

z :: Bool
z = True

f :: Int -> Int
f = \a -> a + 1
-- f a = a + 1

g :: Int -> Int -> Int
-- g a b = a + b
-- g a = \b -> a + b
g = \a -> \b -> a + b

d :: Int
-- d = 1 + 2
--d = (+) 1 2
--d = 1 `g` 2
d = g 1 2

data Personnel = MkPersonnel String Int
    deriving (Show)
p :: Personnel
p = MkPersonnel "Priya" 31

data Bool' = True' | False'
    deriving (Show, Eq)
bb :: Bool'
bb = True'

data Z = X String | Y 
    deriving (Show)

k :: Z -> String
k (X s) = s
k Y = "Y"

-- in pattern matching style above statements could be written like following
l :: Z -> String
l zunc = 
    case zunc of
        X "Priya" -> "Aggarwal"
        X s -> s
        Y -> "Yesss!"

-- implementation of Eq for type Z
instance Eq Z where
    X s1 == X s2 = s1 == s2
    Y == Y = True
    _ == _ = False

class ToString a where
    toString :: a -> String

instance ToString Int where
    toString = show

instance ToString Personnel where
    toString (MkPersonnel "Priya" 31) = "Priya Aggarwal"
    toString _ = "Somebody else"