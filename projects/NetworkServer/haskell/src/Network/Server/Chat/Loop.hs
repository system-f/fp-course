module Network.Server.Chat.Loop where

import Prelude hiding (mapM_, catch)
import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..))
import Data.IORef(IORef, newIORef, readIORef)
import Data.Foldable(Foldable, mapM_)
import Control.Applicative(Applicative, pure)
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, catch, Exception)
import Control.Monad(forever)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))

import Network.Server.Common.Accept
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Env
import Network.Server.Common.Ref
import Data.Set(Set)
import qualified Data.Set as S

data Loop v f a =
  Loop (Env v -> f a)

type IOLoop v a =
  Loop v IO a

type IORefLoop v a =
  IOLoop (IORef v) a

instance Functor f => Functor (Loop v f) where
  fmap f (Loop k) =
    Loop (fmap f . k)

instance Applicative f => Applicative (Loop v f) where
  pure =
    Loop . pure . pure
  Loop f <*> Loop x =
    Loop (\a -> f a <*> x a)

instance Monad f => Monad (Loop v f) where
  return =
    Loop . return . return
  Loop k >>= f =
    Loop (\v -> k v >>= \a ->
      let Loop l = f a
      in l v)

instance MonadTrans (Loop v) where
  lift =
    Loop . const

instance MonadIO f => MonadIO (Loop v f) where
  liftIO =
    lift . liftIO

etry ::
  Exception e =>
  (Env v -> IO a)
  -> IOLoop v (Either e a)
etry k =
  Loop $ try . k

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v () -- per-client
  -> IO a
server i r (Loop f) =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (f (Env q c x))
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

allClients :: IOLoop v (Set Ref)
allClients = Loop $ \env -> readIORef (clientsL `getL` env)

perClient ::
  IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v a) -- read line from client
  -> IOLoop v ()
perClient =
  error "todo"

loop ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v w) -- read line from client
  -> IO a
loop i r q f =
  server i r (perClient q f)

iorefServer ::
  v -- server initialise
  -> IORefLoop v () -- per-client
  -> IO a
iorefServer x =
  server (newIORef x) return

iorefLoop ::
  v -- server initialise
  -> IORefLoop v x -- client accepted (post)
  -> (String -> IORefLoop v w) -- read line from client
  -> IO a
iorefLoop x q f =
  iorefServer x (perClient q f)

pPutStrLn ::
  String
  -> IOLoop v ()
pPutStrLn s =
  Loop (`lPutStrLn` s)

(!) ::
  Foldable t =>
  IOLoop v (t Ref)
  -> String
  -> IOLoop v ()
clients ! msg =
 clients >>= purgeClients (\y -> liftIO (lPutStrLn y msg))

infixl 2 !

purgeClients ::
  Foldable t =>
  (Ref -> IOLoop v ())
  -> t Ref
  -> IOLoop v ()
purgeClients a =
  mapM_ (\y ->
    ecatch (a y)
      (\x -> do _ <- modifyClients (S.delete y)
                xprint x)
        )

readEnv ::
  Applicative f =>
  Loop v f (Env v)
readEnv =
  Loop $ pure

readEnvval ::
  Applicative f =>
  Loop v f v
readEnvval =
  fmap (envvalL `getL`) readEnv

readIOEnvval ::
  IORefLoop a a
readIOEnvval =
  Loop $ \env ->
    readIORef (envvalL `getL` env)

allClientsButThis ::
  IOLoop v (Set Ref)
allClientsButThis =
  Loop $ \env ->
    fmap (S.delete ((acceptL .@ refL) `getL` env)) (readIORef (clientsL `getL` env))

-- Control.Monad.CatchIO
ecatch ::
  Exception e =>
  IOLoop v a
  -> (e -> IOLoop v a)
  -> IOLoop v a
ecatch (Loop k) f =
  Loop $ \env -> k env `catch` (\e -> let Loop l = f e in l env)

modifyClients ::
  (Set Ref -> Set Ref)
  -> IOLoop v (Set Ref)
modifyClients f =
  Loop $ \env ->
    atomicModifyIORef_ (clientsL `getL` env) f
