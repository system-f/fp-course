module Network.Server.Common.Ref where

import Network.Server.Common.HandleLens(HandleLens(..))
import Network.Server.Common.Lens(iso)
import System.IO(Handle)
import Data.Function(on)

newtype Ref =
  Ref Handle
  deriving (Eq, Show)

instance Ord Ref where
  compare =
    compare `on` show

instance HandleLens Ref where
  handleL =
    iso (\(Ref h) -> h) Ref
