module Network.TicTacToe.Ref where

import Network.TicTacToe.HandleLens(HandleLens(..))
import System.IO(Handle)
import Data.Function(on)
import Network.TicTacToe.Lens(iso)

newtype Ref =
  Ref Handle
  deriving (Eq, Show)

instance Ord Ref where
  compare =
    compare `on` show

instance HandleLens Ref where
  handleL =
    iso (\(Ref h) -> h) Ref
