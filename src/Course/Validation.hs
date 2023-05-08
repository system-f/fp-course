{-# LANGUAGE NoImplicitPrelude #-}

module Course.Validation where

import Course.Core
--     ^^^^^^^^^^^ put everything from `Course.Core` into top-level scope.
import Prelude (String)
--     ^^^^^^^^^^^^^^^^ put only `String` from `Prelude` into top-level scope.
--                      nothing else from `Prelude` will be visible in this module.

type Err = String
--         ^^^^^^ 'Err' is now an alias for 'String'. the compiler makes no distinction between the two
--       ^ we know this is a type alias, and not a kind signature, because we use `=` instead of `::`
--   ^^^ type alias. purely for programmer's convenience
-- ^ the `type` keyword is used to define a type alias or a kind signature

-- | A value of type @'Validation' a@ is either
--
--     * @'Error' x@, where @x@ is a value of type 'Err', or
--
--     * @'Value' y@, where @y@ is a value of type @a@.
--
-- A literal translation to Typescript would look something like this.
--
-- > type Err = string
-- > type Validation<A> = ['Error', Err] | ['Value', A]
--
-- A more-idiomatic translation to Typescript would look something like this.
--
-- > type Err = string
-- >
-- > type Validation<A> = _Error<A> | Value<A>
-- >
-- > class _Error<A> {
-- >   constructor(public err: Err) {
-- >     this.err = err
-- >   }
-- > }
-- >
-- > class Value<A> {
-- >   constructor(public value: A) {
-- >     this.value = value
-- >   }
-- > }
--
-- A literal translation to Kotlin would look something like this.
--
-- > typealias Err = String
-- > sealed class Validation<A>()
-- > data class Error<A>(val err: Err) : Validation<A> {}
-- > data class Value<A>(val a: A) : Validation<A> {}
--
-- A translation to Java was not possible until Java 17, with support for sealed classes.
-- I'll omit such an example, though, to keep this file from growing too large.
--
-- All of the above translations are imperfect, however.
-- They all differ from the Haskell version in one crucial way.
-- In each of the other examples, the Error case and the Value case are /types/ in their own right.
-- On the other hand, the Haskell code doesn't define any types named `Error` or `Value`.
-- We're only defining a single type named 'Validation'; 'Error' and 'Value' are that type's /data constructors./
-- 'Error' and 'Value' are not types in their own right.
--
-- A semantically-failfull translation to Typescript would look something like this.
--
-- > type Err = string
-- >
-- > type Validation<A> = {
-- >   match: <T> (
-- >     onError: (err: Err) => T,
-- >     onValue: (value: A) => T
-- >   ) => T
-- > }
-- >
-- > function _Error<A>(err: Err): Validation<A> {
-- >   return {
-- >     match: (onError, _) => onError(err)
-- >   }
-- > }
-- >
-- > function Value<A>(value: A): Validation<A> {
-- >   return {
-- >     match: (_, onValue) => onValue(value)
-- >   }
-- > }
--
-- In this version, the two data constructors are represented by functions instead of by classes.
-- This way `_Error<A>` and `Value<A>` are not types.
-- The `Validation` method `match` takes the place of Haskell's built-in pattern matching feature.
-- The Typescript version uses callbacks to mimic the different branches of a Haskell case expression.
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
--                 ^^^^^^^^^^^^ Technically, `Validation` isn't a type.
--                              `Validation` is a "type constructor". It's a function that takes a type and returns a type.
--                              The result of applying the function `Validation` to the type `a` is the type `Validation a`.
--                              As long as you  understand the distinction between types and type constructors, though,
--                              we'll continue to just call them all "types," because it's shorter.

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

-- Here's another way we could have written `isError`
--
-- > isError :: Validation a -> Bool
-- > isError v = case v of
-- >   Error _ -> True
-- >   Value _ -> False

-- | Question Validation 1
--
--   a) Why do we need to define `isError` for both data constructors? Can't we just define it for `Error`?
--   b) Are `Error` and `Value` types? Are they type constructors?
--   c) What are some of the differences between a type constructor and a data constructor?
question_Validation_1 :: (String, String, String)
question_Validation_1 = error "todo"

-- | Question Validation 1.1
--
-- Implement `isError` in Typescript, using the semantically-failfull translation of `Validation` from above.
question_Validation_1_1 :: String
question_Validation_1_1 =
  error
    "// Haskell supports multi-line strings, but it's ugly :-(\n\
    \\n\
    \function isError<A>(v: Validation<A>): boolean {\n\
    \  error \"todo\"\n\
    \}\n\
    \"

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
-- Partly that's because it seems to be written backwards.
-- Go to [https://hoogle.haskell.org/](https://hoogle.haskell.org/) and search for the function "(.)".
-- Navigate to its documentation, and then navigate to the "Source" link you'll see to the right.
--     a) What's the implementation of `(.)`? What is it doing?
--     b) How is the order of `(.)` arguments related to (or influenced by, maybe) its implementation?
question_Validation_2 :: (String, String)
question_Validation_2 = error "todo"

errorMessage :: Validation a -> String
--                              ^^^^^^ result type
--                         ^ an unconstrained type variable. `errorMessage` is _fully polymorphic_ with respect to `a`
--              ^^^^^^^^^^^^ first (and only) argument
errorMessage v = case v of
  Error s -> s
  Value _ -> "no error message"

-- Here's another way we could have written `errorMessage`
--
-- > errorMessage :: Validation a -> String
-- > errorMessage (Error s) = s
-- > errorMessage _ = "no error message"

valueString :: Show a => Validation a -> String
--                                       ^^^^^^ result type
--                       ^^^^^^^^^^^^ first (and only) argument
--                  ^ a constrained type variable. `valueString` is _constrained polymorphic_ with respect to `a`
--                    At `valueString` call sites, whatever type we use for `a` must satisfy `Show a`.
--                    For example, if we have `foo :: Bar` and we write `valueString (Value foo)` in our code,
--                    then at this use site, the type variable `a` will be instantiated to `Bar`.
--                    The compiler will check to see if `Bar` satisfied `Show Bar`.
--                    If it does, then the compiler will accept the code.
--                    If it doesn't, then the compiler will reject the code.
--             ^^^^^^ function constraints. `Show a` must be satisfied by whatever type we pick for `a` any time we call this function.
valueString v = case v of
  Error _ -> "no value"
  Value x -> show x

-- | Question Validation 2.1
--
-- Load the project in GHCi. Try to call `valueString` with an `Error` like so:
--
-- > data Bar = MakeBar
-- > err = Error "message" :: Validation Bar
-- > valueString err
--
--   a) Does GHC accept the program? What error do you get?
--   b) Why is this error reasonable? Explain.
--   c) Did the error surprise you? Why or why not?
question_Validation_2_1 :: (String, String, String)
question_Validation_2_1 = error "todo"

-- | Maps a function on a validation's value side.
--
--  >>> mapValidation (+10) (Error "message")
--  Error "message"
--
--  >>> mapValidation (+10) (Value 7)
--  Value 17
mapValidation :: (a -> b) -> Validation a -> Validation b
--                     ^ unconstrained type variable. `mapValidation` is fully polymorphic with respect to `b`
--                ^ unconstrained type variable. `mapValidation` is fully polymorphic with respect to `a`
mapValidation f (Value x) = Value (f x)
--                                 ^^^ apply callback. result has type `b`
--                          ^^^^^^^^^^^ construct a `Validation b` using `Value` data constructor. Requires a `b` value as argument.
mapValidation _ (Error s) = Error s
--                                ^ `s :: Err`
--                          ^^^^^^^ construct a `Validation b` using `Error` data constructor. Requires an `Err` value as argument.
--            ^ ignore the callback in the `Error` case.

-- | Question Validation 3
--
-- In the `Error` case of `mapValidation`, the return value looks just like the matched argument.
-- However, invoking the `Error` constructor in the return value costs us a heap allocation.
-- Why not return the argument, intact, instead?
--
-- Haskell has a feature called `@-patterns` that allows us to pattern match an argument while still giving it a name.
--
-- > fooFunc :: Validation a -> Foo
-- > fooFunc errVal@(Error _) = ...
-- >                            ^^^ function body for `Error` case with `errVal :: Validation a` in scope
-- >                 ^^^^^^^ pattern match on `errVal` while still giving it a name
-- >         ^^^^^^ arbitrary variable name
-- > fooFunc valVal@(Value _) = ...
-- >                            ^^^ function body for `Value` case with `valVal :: Validation a` in scope
--
-- This can sometimes be used to make code more performant, because it avoids unnecessary heap allocations.
--
-- Refactor the `Error` case of `mapValidation` using an `@-pattern` so that you can return the argument unchanged.
--   a) What happens when you try to compile the program? What's the error?
--   b) Where you surprised by the error? Explain.
--   c) Why is this error reasonable after all? Explain.
question_Validation_3 :: (String, String, String)
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
errorOr (Value _) e = e

valueValidation :: a -> Validation a
valueValidation = Value
--                ^^^^^ data constructors are functions. `Value :: a -> Validation a`

-- | Question Validation 5
--
-- Load the project in GHCi and ask the compiler for the type of the `Error` data constructor.
--
-- > :t Error
--
--   a) What did the compiler say?
--   b) Was the type what you expected? Why or why not?
question_Validation_5 :: (String, String)
question_Validation_5 = error "todo"

-- $noteToTrainee
-- We're done here! Move on to `[Course.Optional](../Optional.hs)`
