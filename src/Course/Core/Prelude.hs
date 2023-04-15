module Course.Core.Prelude where

import Prelude

mapOptional :: (a -> b) -> Maybe a -> Maybe b
mapOptional = fmap
