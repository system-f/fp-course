-----------------------------------------------------------------------------
-- |
-- Module      :  Test.Framework.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This library deals with the common task of pseudo-random number
-- generation. The library makes it possible to generate repeatable
-- results, by starting with a specified initial random number generator,
-- or to get different results on each run by using the system-initialised
-- generator or by supplying a seed from some other source.
--
-- The library is split into two layers:
--
-- * A core /random number generator/ provides a supply of bits.
--   The class 'RandomGen' provides a common interface to such generators.
--   The library provides one instance of 'RandomGen', the abstract
--   data type 'StdGen'.  Programmers may, of course, supply their own
--   instances of 'RandomGen'.
--
-- * The class 'Random' provides a way to extract values of a particular
--   type from a random number generator.  For example, the 'Float'
--   instance of 'Random' allows one to generate random values of type
--   'Float'.
--
-- This implementation uses the Portable Combined Generator of L'Ecuyer
-- ["Test.Framework.Random\#LEcuyer"] for 32-bit computers, transliterated by
-- Lennart Augustsson.  It has a period of roughly 2.30584e18.
--
-----------------------------------------------------------------------------

{-
We vendor an aggressively-pruned version of the random library for our
property-testing implementation. This allows us to run fp-course with
no dependencies.
-}

module Test.Framework.Random
        ( RandomGen(next, genRange, split)
        , StdGen
        , mkStdGen
        , newStdGen
        , Random ( randomR )
        ) where

import Prelude
import Data.Bits
import Data.Int
import System.CPUTime   ( getCPUTime )
import Data.Time        ( getCurrentTime, UTCTime(..) )
import Data.Ratio       ( numerator, denominator )
import Data.IORef       ( IORef, atomicModifyIORef', newIORef )
import System.IO.Unsafe ( unsafePerformIO )

getTime :: IO (Integer, Integer)
getTime = do
  utc <- getCurrentTime
  let daytime = toRational $ utctDayTime utc
  return $ quotRem (numerator daytime) (denominator daytime)

-- | The class 'RandomGen' provides a common interface to random number
-- generators.
--
-- Minimal complete definition: 'next' and 'split'.
class RandomGen g where

   -- |The 'next' operation returns an 'Int' that is uniformly distributed
   -- in the range returned by 'genRange' (including both end points),
   -- and a new generator.
   next     :: g -> (Int, g)

   -- |The 'genRange' operation yields the range of values returned by
   -- the generator.
   --
   -- It is required that:
   --
   -- * If @(a,b) = 'genRange' g@, then @a < b@.
   --
   -- * 'genRange' always returns a pair of defined 'Int's.
   --
   -- The second condition ensures that 'genRange' cannot examine its
   -- argument, and hence the value it returns can be determined only by the
   -- instance of 'RandomGen'.  That in turn allows an implementation to make
   -- a single call to 'genRange' to establish a generator's range, without
   -- being concerned that the generator returned by (say) 'next' might have
   -- a different range to the generator passed to 'next'.
   --
   -- The default definition spans the full range of 'Int'.
   genRange :: g -> (Int,Int)

   -- default method
   genRange _ = (minBound, maxBound)

   -- |The 'split' operation allows one to obtain two distinct random number
   -- generators. This is very useful in functional programs (for example, when
   -- passing a random number generator down to recursive calls), but very
   -- little work has been done on statistically robust implementations of
   -- 'split' (["Test.Framework.Random\#Burton", "Test.Framework.Random\#Hellekalek"]
   -- are the only examples we know of).
   split    :: g -> (g, g)

{- |
The 'StdGen' instance of 'RandomGen' has a 'genRange' of at least 30 bits.

The result of repeatedly using 'next' should be at least as statistically
robust as the /Minimal Standard Random Number Generator/ described by
["Test.Framework.Random\#Park", "Test.Framework.Random\#Carta"].
Until more is known about implementations of 'split', all we require is
that 'split' deliver generators that are (a) not identical and
(b) independently robust in the sense just given.

The 'Show' and 'Read' instances of 'StdGen' provide a primitive way to save the
state of a random number generator.
It is required that @'read' ('show' g) == g@.

In addition, 'reads' may be used to map an arbitrary string (not necessarily one
produced by 'show') onto a value of type 'StdGen'. In general, the 'Read'
instance of 'StdGen' has the following properties:

* It guarantees to succeed on any string.

* It guarantees to consume only a finite portion of the string.

* Different argument strings are likely to result in different results.

-}
data StdGen
 = StdGen !Int32 !Int32

instance RandomGen StdGen where
  next  = stdNext
  genRange _ = stdRange

  split = stdSplit

mkStdGen :: Int -> StdGen -- why not Integer ?
mkStdGen s = mkStdGen32 $ fromIntegral s

{-
From ["Test.Framework.Random\#LEcuyer"]: "The integer variables s1 and s2 ... must be
initialized to values in the range [1, 2147483562] and [1, 2147483398]
respectively."
-}
mkStdGen32 :: Int32 -> StdGen
mkStdGen32 sMaybeNegative = StdGen (s1+1) (s2+1)
      where
        -- We want a non-negative number, but we can't just take the abs
        -- of sMaybeNegative as -minBound == minBound.
        s       = sMaybeNegative .&. maxBound
        (q, s1) = s `divMod` 2147483562
        s2      = q `mod` 2147483398

createStdGen :: Integer -> StdGen
createStdGen s = mkStdGen32 $ fromIntegral s

{- |
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.

Minimal complete definition: 'randomR' and 'random'.
-}
class Random a where
  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  randomR :: RandomGen g => (a,a) -> g -> (a,g)

instance Random Integer where randomR = randomIvalInteger
instance Random Int     where randomR = randomIvalIntegral

mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (sec, psec) <- getTime
    return (createStdGen (sec * 12345 + psec + ct + o))

-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

{-# SPECIALIZE randomIvalInteger :: (Num a) =>
    (Integer, Integer) -> StdGen -> (a, StdGen) #-}

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q).  Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate random values until we exceed the target magnitude
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))

stdRange :: (Int,Int)
stdRange = (1, 2147483562)

stdNext :: StdGen -> (Int, StdGen)
-- Returns values in the range stdRange
stdNext (StdGen s1 s2) = (fromIntegral z', StdGen s1'' s2'')
        where   z'   | z < 1 = z + 2147483562
                     | otherwise = z
                z    = s1'' - s2''

                k    = s1 `quot` 53668
                s1'  = 40014 * (s1 - k * 53668) - k * 12211
                s1'' | s1' < 0 = s1' + 2147483563
                     | otherwise = s1'

                k'   = s2 `quot` 52774
                s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
                s2'' | s2' < 0 = s2' + 2147483399
                     | otherwise = s2'

stdSplit            :: StdGen -> (StdGen, StdGen)
stdSplit std@(StdGen s1 s2)
                     = (left, right)
                       where
                        -- no statistical foundation for this!
                        left    = StdGen new_s1 t2
                        right   = StdGen t1 new_s2

                        new_s1 | s1 == 2147483562 = 1
                               | otherwise        = s1 + 1

                        new_s2 | s2 == 1          = 2147483398
                               | otherwise        = s2 - 1

                        StdGen t1 t2 = snd (next std)

{-# NOINLINE theStdGen #-}
theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ do
   rng <- mkStdRNG 0
   newIORef rng

-- |Applies 'split' to the current global random generator,
-- updates it with one of the results, and returns the other.
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef' theStdGen split
