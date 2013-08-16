module Network.TicTacToe.HandleLens where

import Network.TicTacToe.Lens(Lens, identityL)
import System.IO(Handle)

class HandleLens a where
  handleL ::
    Lens a Handle

instance HandleLens Handle where
  handleL =
    identityL
