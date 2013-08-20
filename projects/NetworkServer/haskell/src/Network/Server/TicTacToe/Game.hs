module Network.Server.TicTacToe.Game where

import Data.TicTacToe
import qualified Data.TicTacToe as T
import Network.Server.Common.Env
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Ref
import Network.Server.TicTacToe.Loop
import Data.Char(isSpace, toLower, toUpper)
import Data.Function(on)
import Data.IORef(readIORef, atomicModifyIORef)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum, find)
import Data.Set(Set)
import Control.Applicative((<$), (<$>))
import System.IO(hGetLine, hPutStrLn)

type FinishedGames =
  [FinishedBoard]

type Game a =
  IORefLoop Unfinished (Unfinished, FinishedGames) a

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

currentBoard ::
  Game Unfinished
currentBoard =
  initLoop $ \env ->
    readIORef (envvalL `getL` env)

withCurrentBoard ::
  (Unfinished -> (Unfinished, a))
  -> Game a
withCurrentBoard f =
  initLoop $ \env ->
    atomicModifyIORef (envvalL `getL` env) f

lastBoard ::
  Game Unfinished
lastBoard =
  Loop $ \_ (s, t) ->
    return (s, (s, t))

putBoard ::
  Unfinished
  -> Game ()
putBoard s =
  Loop $ \_ (_, t) ->
      return ((), (s, t))

modifyFinishedGames ::
  (FinishedGames -> FinishedGames)
  -> Game ()
modifyFinishedGames f =
  Loop $ \_ (s, t) -> return ((), (s, f t))

finishedGames ::
  Game FinishedGames
finishedGames =
  Loop $ \_ (s, t) -> return (t, (s, t))

eGetLine ::
  Game String
eGetLine =
  initLoop (hGetLine . getL handleL)

ePutStrLn ::
  String
  -> Game ()
ePutStrLn s =
  initLoop (\env -> (hPutStrLn (handleL `getL` env) s))

allClients ::
  Game (Set Ref)
allClients =
  initLoop $ \env -> (readIORef (clientsL `getL` env))

data AtomicMove =
  IsOccupied
  | OutOfDate
  | MoveMade Board
  | GameOver FinishedBoard
  deriving (Eq, Show)

process ::
  Command
  -> Game ()
process (Move p) =
  do l <- lastBoard
     r <- withCurrentBoard $ \b ->
            if isOccupied b p
              then
                (b, IsOccupied)
              else
                if b == l
                  then
                    let r = p --> b
                    in case r of UnemptyBoard b' -> (UnfinishedBoard b', MoveMade b')
                                 UnemptyFinished f -> (UnfinishedEmpty T.empty, GameOver f)
                  else
                    (b, OutOfDate)
     case r of IsOccupied ->
                 ePutStrLn (concat ["MOVE ", show p, " is occupied"])
               OutOfDate  ->
                 ePutStrLn "MOVE board is out of date"
               MoveMade b ->
                 do putBoard (UnfinishedBoard b)
                    ePutStrLn (showBoard b)
               GameOver b ->
                  do modifyFinishedGames (b:)
                     putBoard (UnfinishedEmpty T.empty)
                     allClients ! "MOVE GAME OVER " ++ show (getResult b)
process Current =
  do b <- currentBoard
     putBoard b
     ePutStrLn (showBoard b)
process Finished =
  do g <- finishedGames
     mapM_ (ePutStrLn . showBoard) g
process (Chat m) =
  allClientsButThis ! "CHAT " ++ m
process Turn =
  do b <- currentBoard
     putBoard b
     ePutStrLn [toSymbol (whoseTurn b)]
process (At p) =
  do b <- currentBoard
     putBoard b
     ePutStrLn [fromMaybe '?' . fmap toSymbol . playerAt b $ p]
process (Unknown s) =
  ePutStrLn ("UNKNOWN " ++ s)

game ::
  Game x -- client accepted (post)
  -> (String -> Game w) -- read line from client
  -> IO a
game =
  iorefLoop (UnfinishedEmpty T.empty) (UnfinishedEmpty T.empty, [])

play ::
  IO a
play =
  game (currentBoard >>= pPutStrLn . showBoard) (process . command)

