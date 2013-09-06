module Network.Server.Common.HandleLens where

import Network.Server.Common.Lens(Lens, identityL, getL)
import System.IO(Handle, BufferMode, hGetLine, hPutStrLn, hClose, hSetBuffering)

class HandleLens a where
  handleL ::
    Lens a Handle

instance HandleLens Handle where
  handleL =
    identityL

lGetLine ::
  HandleLens h =>
  h
  -> IO String
lGetLine h =
  hGetLine (handleL `getL` h)

lPutStrLn ::
  HandleLens h =>
  h
  -> String
  -> IO ()
lPutStrLn h =
  hPutStrLn (handleL `getL` h)

lClose ::
  HandleLens h =>
  h
  -> IO ()
lClose h =
  hClose (handleL `getL` h)

lSetBuffering ::
  HandleLens h =>
  h
  -> BufferMode
  -> IO ()
lSetBuffering h =
  hSetBuffering (handleL `getL` h)
