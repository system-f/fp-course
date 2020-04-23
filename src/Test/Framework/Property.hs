-- Small version of Quickcheck inspired by the original papers and
-- implementation:
--
-- https://users.cs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/QuickCheck.hs

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Framework.Property where

import           Prelude

import           Control.Monad
import           Course.List (List, listh)
import           Course.ListZipper (ListZipper(..))
import           Course.Validation (Validation(..))
import           Data.Bool
import           Data.Char
import           Data.String (IsString(..))
import           Data.Tree
import           Test.Framework.Random

newtype Gen a = Gen { runGen :: Int -> StdGen -> a } deriving Functor

instance Applicative Gen where
  pure = Gen . const . const

  Gen g <*> Gen h = Gen $ \size gen ->
    let
      (genG, genH) = split gen
    in
      g size genG $ h size genH

instance Monad Gen where
  Gen g >>= f = Gen $ \size gen ->
    let
      (genG, genA) = split gen
    in
      runGen (f (g size genG)) size genA

generate :: Int -> StdGen -> Gen a -> a
generate size gen (Gen g) = g size gen

choose :: (Random a, Integral a) => (a, a) -> Gen a
choose bounds = Gen . const $ fst . randomR bounds

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

sized :: Integral i => (i -> Gen a) -> Gen a
sized f = Gen $ \size gen -> runGen (f $ fromIntegral size) size gen

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = 
  let tot = sum (fst <$> xs)
      pick n ((k,x):xs)
        | n <= k    = x
        | otherwise = pick (n-k) xs
  in  choose (1, tot) >>= (`pick` xs)

vector :: Arbitrary a => Int -> Gen [a]
vector = (`replicateM` arbitrary)

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen $ \size gen a -> runGen (f a) size gen

variant :: Int -> Gen a -> Gen a
variant v (Gen g) = Gen (\size gen -> g size (rands gen !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0

evaluate :: Testable a => a -> Gen Result
evaluate = runProperty . property

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Property $ do
  a <- gen
  res <- evaluate (body a)
  pure $ argument a res
  where
    argument a res = res { arguments = show a : arguments res }

newtype Positive = Positive { getPositive :: Integer } deriving Show
newtype Unshowable a = Unshowable { getUnshowable :: a } deriving Arbitrary

instance Show (Unshowable a) where
  show _ = "<Unshowable>"

class Arbitrary a where
  arbitrary :: Gen a

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = (,) <$> arbitrary <*> arbitrary

instance Arbitrary Bool where
  arbitrary = elements [True, False]

instance Arbitrary Char where
  arbitrary = choose (32,255) >>= \n -> return (chr n)

instance Arbitrary Int where
  arbitrary = sized $ \size -> choose (-size, size)

instance Arbitrary Integer where
  arbitrary = sized $ \size -> choose (-size, size)

instance Arbitrary Positive where
  arbitrary = Positive <$> sized (\size -> choose (0, size))

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized $ \size -> choose (0, size) >>= vector

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listh <$> arbitrary

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = ListZipper <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary = frequency
    [ (1, pure $ Error "Error")
    , (4, Value <$> arbitrary)
    ]

class Coarbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

instance (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote $ \a -> coarbitrary a arbitrary

instance Coarbitrary Bool where
  coarbitrary = bool (variant 0) (variant 1)

instance Coarbitrary a => Coarbitrary [a] where
  coarbitrary [] = variant 0
  coarbitrary (x:xs) = variant 1 . coarbitrary x . coarbitrary xs

instance (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary f gen = arbitrary >>= \a -> coarbitrary (f a) gen

instance Coarbitrary Int where
  coarbitrary n = variant i where
    i | n >= 0 = 2*n
      | otherwise = 2*(-n) + 1

instance Coarbitrary Integer where
  coarbitrary n = variant (fromInteger i) where
    i | n >= 0 = 2*n
      | otherwise = 2*(-n) + 1

class Testable a where
  property :: a -> Property

instance Testable Property where
  property = id

instance Testable Bool where
  property b = result $ nothing { ok = Just b }

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

data Result = Result
  { ok :: Maybe Bool
  , arguments :: [String]
  }

nothing :: Result
nothing = Result Nothing []

result :: Result -> Property
result = Property . pure

newtype Property = Property { runProperty :: Gen Result }

data Config = Config
  { maxTest :: Int
  , maxFail :: Int
  , resize  :: Int -> Int
  }

quick :: Config
quick = Config
  { maxTest = 100
  , maxFail = 1000
  , resize  = (+ 3) . (`div` 2)
  }

quickCheck :: Testable a => a -> IO ()
quickCheck = check quick

check :: Testable a => Config -> a -> IO ()
check config a =
  newStdGen >>= \gen -> tests config (evaluate a) gen 0 0

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> IO ()
tests config gen rnd0 ntest nfail
  | ntest == maxTest config = putStrLn $ "Passed " ++ show ntest ++ " tests."
  | nfail == maxFail config = putStrLn $ "Exhausted after " ++ show ntest ++ " tests."
  | otherwise = case ok res of
      Nothing -> tests config gen rnd1 ntest (nfail + 1)
      Just True -> tests config gen rnd1 (ntest + 1) nfail
      Just False -> do
        putStrLn $ "Falsifiable after " ++ show ntest ++ " tests:"
        putStr . unlines $ arguments res
    where
      res = generate (resize config ntest) rnd2 gen
      (rnd1, rnd2) = split rnd0
