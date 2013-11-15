{-# LANGUAGE NoImplicitPrelude #-}

module Optional(
  Optional(..)
, length
) where

import qualified Prelude as P

data Optional a = Optional a
data Void

length ::
  Void
length =
  P.undefined

instance P.Monad Optional where
  (>>=) =
    P.undefined
  return =
    P.undefined
