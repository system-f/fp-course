{-# LANGUAGE NoImplicitPrelude #-}

module Course.Validation where

import Course.Core
--     ^^^^^^^^^^^ put everything from `Course.Core` into top-level scope.
import Prelude (String)
--     ^^^^^^^^^^^^^^^^ put only `String` from `Prelude` into top-level scope.
--                      nothing else from `Prelude` will be visible in this module.

type Err = String
--         ^^^^^^ 'Err' is an alias for 'String'. the compiler makes no distinction between the two
--   ^^^ type alias. purely for programmer's convenience

-- | A value of type @'Validation' a@ is either
--
--     * @'Error' x@, where @x@ is a value of type 'Err', or
--
--     * @'Value' y@, where @y@ is a value of type @a@.
--
-- A literal translation to Typescript would look something like this.
--
-- > type Err = string
-- > type Validation<a> = ['Error', Err] | ['Value', a]
--
-- A literal translation to Kotlin would look something like this.
--
-- > typealias Err = String
-- > sealed class Validation<A>()
-- > data class Error<A>(val err: Err) : Validation<A> {}
-- > data class Value<A>(val a: A) : Validation<A> {}
--
-- A translation to Java was not possible until Java 17, with support for sealed classes.
-- I'll omit such and example, though, so that this file won't grow too large.
--
-- All of these translations are imperfect, however.
-- They all differ from the Haskell version in one crucial way.
-- In each of the other examples, the Error case and the Value case are /types./
-- In Haskell, we're not defining any type named `Error` or `Value`.
-- We're defining a type named 'Validation'; 'Error' and 'Value' are that types /data constructors./
-- 'Error' and 'Value' are not types in their own right.
data Validation a = Error Err | Value a
--                                    ^ type of argument to 'Value'
--                              ^^^^^ second data constructor. has one argument
--                        ^^^ type of argument to 'Error'. 'Err' is capitalized, so it's a specific concrete type, rather than a type variable
--                  ^^^^^ first data constructor. has one argument
--              ^ type variable
--   ^^^^^^^^^^ type name
  deriving (Eq, Show)
  --            ^^^^ compiler generates instance 'Show a => Show (Validation a)'
  --        ^^ compiler generates instance 'Eq a => Eq (Validation a)'

type Validation :: Type -> Type

-- | Returns whether or not the given validation is an error.
--
-- >>> isError (Error "message")
-- True
--
-- >>> isError (Value 7)
-- False
isError :: Validation a -> Bool
-- ^^^^ defined using multiple equations
isError (Error _) = True
--       ^^^^^ each equation matches a data constructor of 'Validation'
isError (Value _) = False
--       ^^^^^ taken together, the equations cover all data constructors of 'Validation'

-- | Question Validation 1
--
-- Why do we need to define `isError` for both data constructors? Can't we just define it for `Error`?
question_Validation_1 :: String
question_Validation_1 = error "todo"

-- | Returns whether or not the given validation is a value.
--
-- >>> isValue (Error "message")
-- False
--
-- >>> isValue (Value 7)
-- True
isValue :: Validation a -> Bool
isValue = not . isError
--              ^^^^^^^ second argument to `(.)`. `isError :: Validation a -> Bool`
--            ^ a std lib function. `(.) :: (b -> c) -> (a -> b) -> a -> c`. applied as an infix operator. it's called _function composition._
--        ^^^ first argument to `(.)`. a std lib function. `not :: Bool -> Bool`
--     ^ no argument
-- isValue = (.) not isError
--           ^^^ apply `(.)` in prefix position. literally the same as `not . isError`
--               infix application is _syntax sugar._ the compiler will _desugar_ `not . isError` into `(.) not isError`
-- isValue = \v -> not (isError v)
--           ^^^^^^^^^^^^^^^^^^^^^ define `isValue` using a lambda. equivalent to `not . isError`, but not literally the same

-- | Question Validation 2
--
-- A lot of people feel that the `(.)` function is a bit confusing.
-- Partly that's because it seems to be writtin backwards.
-- Go to [https://hoogle.haskell.org/](https://hoogle.haskell.org/) and search for the function "(.)".
-- Navigate to its documentation, and then navigate to the "Source" link you'll see to the right.
--     a) What's the implementation of `(.)`? What is it doing?
--     b) How is the order of `(.)` arguments related to (or influenced by, maybe) its implementation?
question_Validation_2 :: (String, String)
question_Validation_2 =
  ( error "todo"
  , error "todo"
  )

-- | Maps a function on a validation's value side.
--
--  >>> mapValidation (+10) (Error "message")
--  Error "message"
--
--  >>> mapValidation (+10) (Value 7)
--  Value 17
mapValidation :: (a -> b) -> Validation a -> Validation b
--                     ^ unconstrained type variable. `mapValidation` is fully polymorphic with respect to `b`
--                ^ unconstrained type variable. `mapValidation` is _fully polymorphic_ with respect to `a`
--               ^^^^^^^^ type of first argument
mapValidation _ (Error s) = Error s
--                                ^ constructor argument. `s` is a value of type `Err`
--                          ^^^^^ construct a value of type `Validation b` using the `Error` constructor
--                     ^ the data carried inside the `Error` constructor. `s :: Err`
--            ^ ignore callback in the `Error` case.
mapValidation f (Value x) = Value (f x)
--                                 ^^^ apply callback to the data carried by the `Value` constructor in that case
--                                ^^^^^ constructor argument. Since `f :: a -> b` and `x :: a`, `f a :: b`
--                          ^^^^^ construct a value of type `Validation b` using the `Value` constructor
--                     ^ the data carried by the `Value` constructor. `x :: a`

-- | Question Validation 3
--
-- Why does the implementation of `mapValidation` ignore the first argument when the validation is an error?
question_Validation_3 :: String
question_Validation_3 = error "todo"

-- | Binds a function on a validation's value side to a new validation.
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
-- Error "message"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
-- Error "odd"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
-- Value 18
bindValidation :: (a -> Validation b) -> Validation a -> Validation b
--                ^^^^^^^^^^^^^^^^^^^ first argument, a callback that can accept an `a` and yields a `Validation b`.
bindValidation _ (Error s) = Error s
bindValidation f (Value x) = f x
--                           ^^^ apply `f` to the data carried by the `Value` constructor.

-- | Question Validation 4
--
-- Why doesn't `bindValidation` use a `Value` constructor to wrap the result of `f x`?
question_Validation_4 :: String
question_Validation_4 = error "todo"

-- | Returns a validation's value side or the given default if it is an error.
--
--  >>> valueOr (Error "message") 3
--  3
--
--  >>> valueOr (Value 7) 3
--  7
valueOr :: Validation a -> a -> a
--                         ^ a fallback value of type `a`
--         ^^^^^^^^^^^^ potentially some data of type `a`, but potentially not.
valueOr (Error _) a = a
--                    ^ return the fallback value
--             ^ ignore the error message
--       ^^^^^^^ in case there's no data of type `a`
valueOr (Value a) _ = a
--                    ^ return the data that we found inside the `Value` constructor.
--                ^ ignore the fallback value
--             ^ give it a name
--       ^^^^^^^ in case there is data of type `a`

-- | Returns a validation's error side or the given default if it is a value.
--
-- >>> errorOr (Error "message") "q"
-- "message"
--
-- >>> errorOr (Value 7) "q"
-- "q"
errorOr :: Validation a -> Err -> Err
errorOr (Error e) _ = e
errorOr (Value _) a = a

valueValidation :: a -> Validation a
valueValidation = Value
--                ^^^^^ data constructors are functions. `Value :: a -> Validation a`

-- | Question Validation 5
--
-- What is the type of the `Error` data constructor?
question_Validation_5 :: String
question_Validation_5 = error "todo"

-- $noteToTrainee
-- We're done here! Move on to `[Course.Optional](../Optional.hs)`
