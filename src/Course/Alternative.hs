{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Alternative where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Optional
import Course.Parser
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Alternative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. empty <|> x = x`
--
-- * The law of right identity
--   `∀x. x <|> empty = x`
--
-- * The law of associativity
--   `∀u v w. u <|> (v <|> w) = (u <|> v) <|> w`

class Applicative k => Alternative k where
  zero ::
    k a
  (<|>) ::
    k a
    -> k a
    -> k a

infixl 3 <|>

-- | Return the first full Optional
--
-- >>> Full 3 <|> zero
-- Full 3
--
-- >>> zero <|> Full 4
-- Full 4
--
-- >>> Full 3 <|> Full 4
-- Full 3
instance Alternative Optional where
  zero ::
    Optional a
  zero =
    error "todo: Course.Alternative zero#instance Optional"
  (<|>) ::
    Optional a
    -> Optional a
    -> Optional a
  (<|>) =
    error "todo: Course.Alternative (<|>)#instance Optional"

-- | Append the lists
--
-- >>> 3 :. 4 :. 5 :. Nil <|> Nil
-- [3,4,5]
--
-- >>> Nil <|> 6 :. 7 :. 8 :. Nil
-- [6,7,8]
--
-- >>> 3 :. 4 :. 5 :. Nil <|> 6 :. 7 :. 8 :. Nil
-- [3,4,5,6,7,8]
instance Alternative List where
  zero ::
    List a
  zero =
    error "todo: Course.Alternative zero#instance List"
  (<|>) ::
    List a
    -> List a
    -> List a
  (<|>) =
    error "todo: Course.Alternative (<|>)#instance List"

-- | Choose the first succeeding parser
--
-- /Tip:/ Check Parser.hs
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') "abc"
-- Result >abc< 'v'
instance Alternative Parser where
  zero ::
    Parser a
  zero =
    error "todo: Course.Alternative zero#instance Parser"
  (<|>) ::
    Parser a
    -> Parser a
    -> Parser a
  (<|>) =
    error "todo: Course.Alternative (<|>)#instance Parser"

-- | Run the provided Alternative action zero or more times, collecting
-- a list of the results.
--
-- /Tip:/ Use @some@, @pure@ and @(<|>)@.
--
-- >>> parse (many character) ""
-- Result >< ""
--
-- >>> parse (many digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (many digit) "abc"
-- Result >abc< ""
--
-- >>> parse (many character) "abc"
-- Result >< "abc"
--
-- >>> parse (many (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (many (character *> valueParser 'v')) ""
-- Result >< ""
many :: Alternative k => k a -> k (List a)
many =
  error "todo: Course.Alternative many"

-- | Run the provided Alternative action one or more times, collecting
-- a list of the results.
--
-- /Tip:/ Use @(:.)@ and @many@.
--
-- >>> parse (some (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (some (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (some (character *> valueParser 'v')) "")
-- True
some :: Alternative k => k a -> k (List a)
some =
  error "todo: Course.Alternative some"

-- | Combine a list of alternatives
--
-- >>> aconcat (Nil :: List (List Int))
-- []
--
-- >>> aconcat ((3:.4:.Nil) :. Nil :. (5:.6:.Nil) :. Nil
-- [3,4,5,6]

-- >>> aconcat (Empty :. Empty :. Full 7 :. Empty :. Full 8 :. Empty :. Nil)
-- Full 7
aconcat :: Alternative k => List (k a) -> k a
aconcat =
  error "todo: Course.Alternative aconcat"
