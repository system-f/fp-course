module Main (
  main
) where

import Network.Server.Chat.Chat
import Network.Server.Chat.Loop

main ::
  IO a
main =
  chat
