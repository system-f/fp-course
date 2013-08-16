module Network.TicTacToe.Command where

import Prelude hiding (elem)
import Data.TicTacToe
import Data.Char(toUpper, isSpace, toLower)
import Data.Function(on)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum, find, elem)
import Control.Applicative((<$), (<$>))

data Command =
  Move Position
  | Current
  | Finished
  | Chat String
  | Turn
  | At Position
  | Unknown String
  deriving (Eq, Show)

-- |
--
-- >>> command "MOVE ne"
-- Move NE
--
-- >>> command "MOVE 2"
-- Move N
--
-- >>> command "GAME"
-- Current
--
-- >>> command "FiniSHED"
-- Finished
--
-- >>> command "CHAT hi"
-- Chat "hi"
--
-- >>> command "Turn"
-- Turn
--
-- >>> command "At 4"
-- At W
--
-- >>> command "At C"
-- At C
--
-- >>> command "At X"
-- Unknown "At X"
--
-- >>> command "Move i"
-- Unknown "Move i"
command ::
  String
  -> Command
command z =
  let p l = reverse . dropWhile isSpace . reverse . dropWhile isSpace <$> prefixThen ((==) `on` toLower) l z
  in Unknown z `fromMaybe` msum [
                                  do m <- p "MOVE "
                                     q <- sPosition m
                                     return (Move q)
                                , Current <$ p "GAME"
                                , Finished <$ p "FINISHED"
                                , Chat <$> p "CHAT"
                                , Turn <$ p "TURN"
                                , do a <- p "AT"
                                     q <- sPosition a
                                     return (At q)
                                ]

-- |
--
-- >>> sPosition "1"
-- Just NW
--
-- > sPosition "E"
-- Just E
--
-- > sPosition "sw"
-- Just SW
--
-- > sPosition "x"
-- Nothing
sPosition ::
  String
  -> Maybe Position
sPosition s =
  let table = [
                (
                  ["1", "NW"]
                , NW
                )
              , (
                  ["2", "N"]
                , N
                )
              , (
                  ["3", "NE"]
                , NE
                )
              , (
                  ["4", "W"]
                , W
                )
              , (
                  ["5", "C"]
                , C
                )
              , (
                  ["6", "E"]
                , E
                )
              , (
                  ["7", "SW"]
                , SW
                )
              , (
                  ["8", "S"]
                , S
                )
              , (
                  ["9", "SE"]
                , SE
                )
              ]
      toUppers = map toUpper
  in fmap snd . find (\(t, _) -> elem (toUppers s) (toUppers <$> t)) $ table

-- |
--
-- >>> prefixThen (==) "ABC" "AB"
-- Nothing
--
-- >>> prefixThen (==) "ABC" "ABC"
-- Just ""
--
-- >>> prefixThen (==) "ABC" "ABCDEF"
-- Just "DEF"
prefixThen ::
  (a -> a -> Bool)
  -> [a]
  -> [a]
  -> Maybe [a]
prefixThen _ [] r =
  Just r
prefixThen _ _ [] =
  Nothing
prefixThen e (a:b) (c:d) =
  if e a c
    then
      prefixThen e b d
    else
      Nothing
