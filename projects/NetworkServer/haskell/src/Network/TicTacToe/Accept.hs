module Network.TicTacToe.Accept where

import Network.TicTacToe.Lens
import Network.TicTacToe.HandleLens(HandleLens(..))
import Network.TicTacToe.Ref(Ref(..))
import Network(HostName, Socket, PortNumber, accept)

data Accept =
  Accept
    Ref
    HostName
    PortNumber
  deriving (Eq, Ord, Show)

refL ::
  Lens Accept Ref
refL =
  Lens
    (\(Accept _ nam num) hd -> Accept hd nam num)
    (\(Accept hd _ _) -> hd)

hostNameL ::
  Lens Accept HostName
hostNameL =
  Lens
    (\(Accept hd _ num) nam -> Accept hd nam num)
    (\(Accept _ nam _) -> nam)

portNumberL ::
  Lens Accept PortNumber
portNumberL =
  Lens
    (\(Accept hd nam _) num -> Accept hd nam num)
    (\(Accept _ _ num) -> num)

instance HandleLens Accept where
  handleL =
    refL .@ handleL

accept' ::
  Socket
  -> IO Accept
accept' =
  fmap (\(hd, nam, num) -> Accept (Ref hd) nam num) . accept
