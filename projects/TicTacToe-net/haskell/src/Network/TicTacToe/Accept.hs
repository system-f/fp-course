module Network.TicTacToe.Accept where

import Network.TicTacToe.HandleLens(HandleLens(..))
import Network.TicTacToe.Ref(Ref(..))
import Network(HostName, Socket, PortNumber, accept)

data Accept =
  Accept
    Ref
    HostName
    PortNumber
  deriving (Eq, Ord, Show)

instance HandleLens Accept where
  handleL =
    error "todo"

accept' ::
  Socket
  -> IO Accept
accept' =
  fmap (\(hd, nam, num) -> Accept (Ref hd) nam num) . accept
