module Data.TicTacToe where

import           Prelude hiding (any, all, mapM, concat)
import qualified GHC.OldList as L
import           Test.QuickCheck
import           Data.Map (Map, singleton)
import qualified Data.Map as M
import           Data.Traversable
import           Data.Foldable hiding (foldr)
import           Data.List(intercalate)


data Player =
    Naught
  | Cross
  deriving (Ord, Eq)

instance Show Player where
  show Naught =
    "O"
  show Cross =
    "X"

data EmptyBoard =
  EmptyBoard
  deriving (Eq, Show)

data Board =
  Board (Map Position Player) [(Position, Player)]
  deriving Eq

data FinishedBoard =
  FinishedBoard (Maybe Player) Board
  deriving Eq

instance Show FinishedBoard where
  show (FinishedBoard Nothing b) =
    show b ++ "\nDraw"
  show (FinishedBoard (Just p) b) =
    show b ++ "\n" ++ show p ++ " wins"

data Position =
  N | E | S | W | NE | NW | SE | SW | C
  deriving (Ord, Eq, Show)

data Outcome =
    InvalidMove
  | Inplay Board
  | Done FinishedBoard
  deriving Eq

instance Show Outcome where
  show InvalidMove =
    "?"
  show (Inplay b) =
    show b
  show (Done b) =
    show b


instance Show Board where
  show (Board m _) =
    let p x = case M.lookup x m of
                Nothing -> ' '
                Just Naught -> 'O'
                Just Cross -> 'X'
        line a b c =
          concat
             [
               "| "
             , [p a]
             , " | "
             , [p b]
             , " | "
             , [p c]
             , " |"
             ]
        blank = ".===.===.===."
    in intercalate "\n"
         [
           blank
         , line NW N NE
         , blank
         , line W C E
         , blank
         , line SW S SE
         , blank
         ]

start ::
  Position
  -> Board
start p =
  Board (singleton p Naught) [(p, Naught)]

move'' ::
  Outcome
  -> Position
  -> Outcome
move'' InvalidMove _ =
  InvalidMove
move'' (Inplay b) p =
  move b p
move'' (Done b) _ =
  Done b

data TakenBack =
  BoardBack Board
  | EmptyBack
  deriving (Eq, Show)

takeBack' ::
  FinishedBoard
  -> Board
takeBack' =
  undefined

takeBack ::
  Board
  -> TakenBack
takeBack =
  undefined

move ::
  Board
  -> Position
  -> Outcome
move board@(Board m h) p =
  if p `M.member` m
    then
      InvalidMove
    else
      let m' = M.insert p player m
          wins =
              [
                (NW, N, NE)
              , (N,  C, S)
              , (NE, E, SE)
              , (NW, N, NE)
              , (W, C, E)
              , (SW, S, SE)
              , (NW, C, SE)
              , (SW, C, NE)
              ]
          allEqual (a:b:t) = a == b && allEqual (b:t)
          allEqual _ = True
          isDraw = M.size m' >= 9
          isWin = any (\(a, b, c) -> any allEqual $ mapM (`M.lookup` m') [a, b, c]) wins
          player = whoseTurn board
          b' = Board m' ((p, player):h)
      in
        if isWin
          then Done (FinishedBoard (Just player) b')
          else
            if isDraw
              then Done (FinishedBoard Nothing b')
              else Inplay b'

switch :: Player -> Player
switch Cross = Naught
switch Naught = Cross

whoseTurn :: Board -> Player
whoseTurn (Board _ ((_, p):_)) =
  switch p
whoseTurn (Board _ []) =
  error "broke it"

-- cabal install QuickCheck
instance Arbitrary Position where
  arbitrary = elements [N, NE, NW, S, SE, SW, E, W, C]

instance Arbitrary Player where
  arbitrary = elements [Naught, Cross]

instance Arbitrary Board where
  arbitrary = do
    p <- arbitrary
    ps <- arbitrary
    return $ L.foldr propell (start p) ps

propell :: Position -> Board -> Board
propell p b =
  case move b p of
    InvalidMove -> b
    Done _ -> b
    Inplay b' -> b'

data Blah = Blah Player Position Int

instance Arbitrary Blah where
  arbitrary = do
    player <- arbitrary
    position <- arbitrary
    n <- choose (1, 9)
    return $ Blah player position n

prop_eqp :: Position -> Bool
prop_eqp n = n == n

prop_eq :: Int -> Bool
prop_eq n = n == n

prop_switch :: Player -> Bool
prop_switch p = switch p /=  p

main :: IO ()
main = do
  quickCheck prop_eqp
  quickCheck prop_eq
  quickCheck prop_switch
