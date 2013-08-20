module Network.Server.Common.Env where

import Network.Server.Common.Accept
import Network.Server.Common.Ref
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Data.IORef(IORef, atomicModifyIORef)
import Data.Set(Set)

data Env a =
  Env
    Accept
    (IORef (Set Ref))
    a
  deriving Eq

acceptL ::
  Lens (Env a) Accept
acceptL =
  Lens
    (\(Env _ s a) x -> Env x s a)
    (\(Env x _ _) -> x)

clientsL ::
  Lens (Env a) (IORef (Set Ref))
clientsL =
  Lens
    (\(Env x _ a) s -> Env x s a)
    (\(Env _ s _) -> s)

envvalL ::
  Lens (Env a) a
envvalL =
  Lens
    (\(Env x s _) a -> Env x s a)
    (\(Env _ _ a) -> a)

instance HandleLens (Env a) where
  handleL =
    acceptL .@ handleL

instance Functor Env where
  fmap f (Env x s a) =
    Env x s (f a)

atomicModifyIORef_ ::
  IORef a
  -> (a -> a)
  -> IO a
atomicModifyIORef_ r f =
  atomicModifyIORef r (\a -> (f a, a))
