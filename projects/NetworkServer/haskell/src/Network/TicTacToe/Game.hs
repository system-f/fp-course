module Network.TicTacToe.Game where

import Prelude hiding (elem, mapM_, concat, catch)

import Network.TicTacToe.Lens
import Data.TicTacToe
import qualified Data.TicTacToe as T

import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..), hSetBuffering, hClose, hPutStrLn, hGetLine)
import Data.Maybe(fromMaybe)
import Data.IORef(IORef, readIORef, newIORef, atomicModifyIORef)
import Control.Concurrent(forkIO)
import Control.Monad(forever, liftM)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))
import Control.Applicative(Applicative(..))
import Control.Exception(IOException, Exception, finally, catch, try)
import Data.Foldable(Foldable, mapM_, concat)
import Data.Set(Set)
import qualified Data.Set as S

import Network.TicTacToe.HandleLens(HandleLens(..), lGetLine, lPutStrLn, lSetBuffering)
import Network.TicTacToe.Accept(accept', refL)
import Network.TicTacToe.Command
import Network.TicTacToe.Ref(Ref)
import Network.TicTacToe.Env(FinishedGames, Env(..), boardrefL, boardL, finishedGamesL, clientsL, acceptL)

newtype Game f a =
  Game (Env -> f (a, Unfinished, FinishedGames))

fGame ::
  Functor f =>
  (Env -> f (a, Unfinished))
  -> Game f a
fGame f =
  Game (\env -> fmap (\(a, b) -> (a, b, finishedGamesL `getL` env)) . f $ env)

fGame' ::
  Monad f =>
  (Env -> f (a, Unfinished))
  -> Game f a
fGame' f =
  Game (\env -> liftM (\(a, b) -> (a, b, finishedGamesL `getL` env)) . f $ env)

rGame ::
  Functor f =>
  (Env -> f a)
  -> Game f a
rGame f =
  fGame (\env -> fmap (\a -> (a, boardL `getL` env)) (f env))

rGame' ::
  Monad f =>
  (Env -> f a)
  -> Game f a
rGame' f =
  fGame' (\env -> liftM (\a -> (a, boardL `getL` env)) . f $ env)

idGame ::
  Applicative f =>
  Game f Env
idGame =
  rGame pure

instance Functor f => Functor (Game f) where
  fmap f (Game k) =
    Game (fmap (\(a, b, g) -> (f a, b, g)) . k)

instance Monad f => Monad (Game f) where
  return =
    rGame' . return . return
  Game k >>= f =
    Game (\env -> k env >>= \(a, b, g) -> let Game l = f a
                                          in l (setL finishedGamesL (setL boardL env b) g))

instance MonadTrans Game where
  lift =
    rGame' . const

instance MonadIO f => MonadIO (Game f) where
  liftIO =
    lift . liftIO

xprint ::
  IOException
  -> Game IO ()
xprint =
  liftIO . print

atomicModifyIORef_ ::
  IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef_ r f =
  atomicModifyIORef r (\a -> (f a, ()))

data AtomicMove =
  IsOccupied
  | OutOfDate
  | MoveMade Board
  | GameOver FinishedBoard
  deriving (Eq, Show)


-- Control.Monad.CatchIO
ecatch ::
  Exception e =>
  Game IO a
  -> (e -> Game IO a)
  -> Game IO a
ecatch (Game k) f =
  Game $ \env -> k env `catch` (\e -> let Game l = f e in l env)

etry ::
  Exception e =>
  (Env -> IO a)
  -> Game IO (Either e a)
etry k =
  rGame $ try . k

allClients ::
  Game IO (Set Ref)
allClients =
  rGame $ \env -> (readIORef (clientsL `getL` env))

allClientsButThis ::
  Game IO (Set Ref)
allClientsButThis =
  rGame $ \env ->
    fmap (S.delete ((acceptL .@ refL) `getL` env)) (readIORef (clientsL `getL` env))

modifyClients ::
  (Set Ref -> Set Ref)
  -> Game IO ()
modifyClients f =
  rGame $ \env ->
    atomicModifyIORef_ (clientsL `getL` env) f

modifyFinishedGames ::
  Applicative f =>
  (FinishedGames -> FinishedGames)
  -> Game f ()
modifyFinishedGames f =
  Game $ \env -> pure ((), boardL `getL` env, f (finishedGamesL `getL` env))

finishedGames ::
  Applicative f =>
  Game f FinishedGames
finishedGames =
  rGame $ \env -> pure (finishedGamesL `getL` env)

(!) ::
  Foldable t =>
  Game IO (t Ref)
  -> String
  -> Game IO ()
clients ! msg =
 clients >>= purgeClients (\y -> liftIO (lPutStrLn y msg))

infixl 2 !

purgeClients ::
  Foldable t =>
  (Ref -> Game IO ())
  -> t Ref
  -> Game IO ()
purgeClients a =
  mapM_ (\y ->
    ecatch (a y)
      (\x -> do _ <- modifyClients (S.delete y)
                xprint x)
        )

currentBoard ::
  Game IO Unfinished
currentBoard =
  rGame $ \env ->
    readIORef (boardrefL `getL` env)

withCurrentBoard ::
  (Unfinished -> (Unfinished, a))
  -> Game IO a
withCurrentBoard f =
  rGame $ \env ->
    atomicModifyIORef (boardrefL `getL` env) f

lastBoard ::
  Applicative f =>
  Game f Unfinished
lastBoard =
  rGame $ \env ->
    pure (boardL `getL` env)

putBoard ::
  Applicative f =>
  Unfinished
  -> Game f ()
putBoard =
  fGame . pure . pure . (,) ()

eGetLine ::
  Game IO String
eGetLine =
  rGame (hGetLine . getL handleL)

ePutStrLn ::
  String
  -> Game IO ()
ePutStrLn s =
  rGame (\env -> (hPutStrLn (handleL `getL` env) s))

eClose ::
  Game IO ()
eClose =
  rGame (hClose . getL handleL)

eSetBuffering ::
  BufferMode
  -> Game IO ()
eSetBuffering s =
  rGame (\env -> (hSetBuffering (handleL `getL` env) s))

processCommand ::
  Command
  -> Game IO ()
processCommand (Move p) =
  do l <- lastBoard
     r <- withCurrentBoard $ \b ->
            if isOccupied b p
              then
                (b, IsOccupied)
              else
                if b == l
                  then
                    let r = p --> b
                    in case r of UnemptyBoard b' -> (UnfinishedBoard b', MoveMade b')
                                 UnemptyFinished f -> (UnfinishedEmpty T.empty, GameOver f)
                  else
                    (b, OutOfDate)
     case r of IsOccupied ->
                 ePutStrLn (concat ["MOVE ", show p, " is occupied"])
               OutOfDate  ->
                 ePutStrLn "MOVE board is out of date"
               MoveMade b ->
                 do putBoard (UnfinishedBoard b)
                    ePutStrLn (showBoard b)
               GameOver b ->
                  do modifyFinishedGames (b:)
                     putBoard (UnfinishedEmpty T.empty)
                     allClients ! "MOVE GAME OVER " ++ show (getResult b)
processCommand Current =
  do b <- currentBoard
     putBoard b
     ePutStrLn (showBoard b)
processCommand Finished =
  do g <- finishedGames
     mapM_ (ePutStrLn . showBoard) g
processCommand (Chat m) =
  allClientsButThis ! "CHAT " ++ m
processCommand Turn =
  do b <- currentBoard
     putBoard b
     ePutStrLn [toSymbol (whoseTurn b)]
processCommand (At p) =
  do b <- currentBoard
     putBoard b
     ePutStrLn [fromMaybe '?' . fmap toSymbol . playerAt b $ p]
processCommand (Unknown s) =
  ePutStrLn ("UNKNOWN " ++ s)

server ::
  Game IO ()
  -> IO ()
server (Game g) =
  let hand s b c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        e <- readIORef b
                        forkIO (fmap (\(a, _, _) -> a) $ g (Env q e b c []))
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       b <- newIORef (UnfinishedEmpty T.empty)
       c <- newIORef S.empty
       hand s b c `finally` sClose s

game ::
  Game IO ()
game =
  let loop = do k <- etry lGetLine
                case k of Left e -> xprint e
                          Right [] -> loop
                          Right l -> processCommand (command l) >> loop
  in do b <- lastBoard
        ePutStrLn (showBoard b)
        loop
