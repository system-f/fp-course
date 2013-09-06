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
  Lens
    (\(Env _ e b s f) a -> Env a e b s f)
    (\(Env a _ _ _ _) -> a)

boardL ::
  Lens Env Unfinished
boardL =
  Lens
    (\(Env a _ b s f) e -> Env a e b s f)
    (\(Env _ e _ _ _) -> e)

boardrefL ::
  Lens Env (IORef Unfinished)
boardrefL =
  Lens
    (\(Env a e _ s f) b -> Env a e b s f)
    (\(Env _ _ b _ _) -> b)

clientsL ::
  Lens Env (IORef (Set Ref))
clientsL =
  Lens
    (\(Env a e b _ f) s -> Env a e b s f)
    (\(Env _ _ _ s _) -> s)

finishedGamesL ::
  Lens Env FinishedGames
finishedGamesL =
  Lens
    (\(Env a e b s _) f -> Env a e b s f)
    (\(Env _ _ _ _ f) -> f)

instance HandleLens Env where
  handleL =
    acceptL .@ handleL
