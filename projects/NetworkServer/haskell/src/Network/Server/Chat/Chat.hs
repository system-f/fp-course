module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Data.IORef(atomicModifyIORef)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type Chat a =
  IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef e (\n -> (n + 1, n + 1))

chat ::
  IO a
chat =
  iorefLoop 0 (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Incr <$ trimPrefixThen "INCR" z
                             ]

process ::
  ChatCommand
  -> Chat ()
process =
  error "todo"
