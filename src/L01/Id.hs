module L01.Id where

data Id a = Id a deriving (Eq, Show)

runId :: Id a -> a
runId (Id a) = a

mapId :: (a -> b) -> Id a -> Id b
mapId f (Id a)    = Id (f a)

bindId :: Id a -> (a -> Id b) -> Id b
bindId (Id a) f = f a
