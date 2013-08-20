module Network.TicTacToe.Env where

import Data.TicTacToe
import Network.TicTacToe.Lens
import Network.TicTacToe.HandleLens(HandleLens(..))
import Network.TicTacToe.Accept(Accept)
import Network.TicTacToe.Ref(Ref)
import Data.IORef(IORef)
import Data.Set(Set)

type FinishedGames =
  [FinishedBoard]

data Env =
  Env
    Accept
    Unfinished
    (IORef Unfinished)
    (IORef (Set Ref))
    FinishedGames
  deriving Eq

acceptL ::
  Lens Env Accept
acceptL =
  error "todo"

boardL ::
  Lens Env Unfinished
boardL =
  error "todo"

boardrefL ::
  Lens Env (IORef Unfinished)
boardrefL =
  error "todo"

clientsL ::
  Lens Env (IORef (Set Ref))
clientsL =
  error "todo"

finishedGamesL ::
  Lens Env FinishedGames
finishedGamesL =
  error "todo"

instance HandleLens Env where
  handleL =
    error "todo"
