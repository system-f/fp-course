{-# LANGUAGE NoImplicitPrelude #-}

module Course.TestHelpers where

import Course.Core (Integer, (+))

-- Specialise on Integer to avoid type annotations everywhere
(+:) :: Integer -> Integer -> Integer
(+:) = (+)
