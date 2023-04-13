{-# LANGUAGE NoImplicitPrelude #-}
--           ^^^^^^^^^^^^^^^^^ Compiler option that disables the default import of a module named `Prelude`
{-# LANGUAGE AllowAmbiguousTypes #-}
--           ^^^^^^^^^^^^^^^^^^^ Compiler option. It's not important what it does, and it's not as scary as it sounds.

module Course.ExactlyOne where
--     ^^^^^^^^^^^^^^^^^ our name for this module. it must match the directory/file names

import Course.Core
--     ^^^^^^^^^^^ import all types and functions from module `Course.Core` into top-level scope

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Prelude qualified as P
--                          ^ a name we choose for said namespace
--             ^^^^^^^^^ import types and functions from `Prelude` into an isolated scope/namespace
--     ^^^^^^^ a std lib module. usually imported by default, but we disabled it on line 2

data ExactlyOne a = ExactlyOne a deriving Eq
--                                        ^^ a type class for types that allow equality comparison
--                               ^^^^^^^^ the compiler can write some boilerplate code for us
--                             ^ type of the first (and only) field/argument to `ExactlyOne`
--                  ^^^^^^^^^^ data constructor name (doesn't have to match type name)
--              ^ type variable/generic type parameter
--   ^^^^^^^^^^ type name

runExactlyOne :: ExactlyOne a -> a
--                               ^ type of return value
--                          ^ type variable. this function is _polymorphic_
--               ^^^^^^^^^^^^ type of first (and only) argument
--            ^^ start of type signature
-- ^^^^^^^^^^ function declaration
runExactlyOne (ExactlyOne a0) = a0
--                              ^^ function body
--                        ^^ assign a name to the contents of 'ExactlyOne's field
--             ^^^^^^^^^^ pattern-match the 'ExactlyOne' data constructor
--            ^^^^^^^^^^^^^^ first argument
-- ^^^^^^^^^^ function definition

mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
--                                           ^^^^^^^^^^^^ type of return value
--                           ^^^^^^^^^^^^ type of second argument
--                     ^ another type variable
--                ^ type variable
--               ^^^^^^^^ type of first argument, a function from 'a' to 'b'
mapExactlyOne f (ExactlyOne a0) = ExactlyOne (f a0)
--                                            ^^^^ apply function 'f' to 'a0'
--                                           ^^^^^^ value for 'ExactlyOne's field
--                                ^^^^^^^^^^ data constructor
--                                ^^^^^^^^^^^^^^^^^ function body
--              ^^^^^^^^^^^^^^^ second argument
--            ^ first argument, a function

bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
--                                 ^ another type variable
--                 ^ type variable
--                                                       ^^^^^^^^^^^^ type of return value
--                                       ^^^^^^^^^^^^ type of second argument
--                ^^^^^^^^^^^^^^^^^^^ type of first argument, a function that returns an 'ExactlyOne' of 'b'
bindExactlyOne f (ExactlyOne a0) = f a0
--                                 ^^^^ apply 'f' to 'a0'
--                ^^^^^^^^^^^^^ pattern match second argument
--             ^ first argument

-- | Question ExactlyOne.1
--
-- I have a function `f :: String -> ExactlyOne Int`. I have a value `x :: ExactlyOne String`.
--   a) What is the type of `mapExactlyOne f x`?
--   b) What is the type of `bindExactlyOne f x`?
question_ExactlyOne_1 :: (P.String, P.String)
question_ExactlyOne_1 =
  ( error "todo"
  , error "todo"
  )

class HasExactlyOne t a where
--                    ^ second type variable
--                  ^ first type variable
--    ^^^^^^^^^^^^^ type class name
-- ^^ the term "class" means something very different in Haskell than in most other languages.
--    For one, a Haskell class is not a type. A Haskell class expresses a property about one or more types.
--    They're kinda like predicates, boolean-valued functions, only their arguments are types instead of values.
--    The compiler will check to see if `HasExactlyOne t a` is true for various specific choices of `t` and `a` for various different reasons.
--    For example, `HasExactlyOne String Int` might or might not be true (spoiler, it's not true).
--    `HasExactlyOne (ExactlyOne Int) Int` happens to be true, because of the various instances we wrote below.
--    So is `HasExactlyOne (ExactlyOne Double, String) Double`, again, because of the instances we wrote.
--    Bottom line is that classes in Haskell are not types: they're a mechanism for expressing properties about types.
--    For this reason, we often use the more-verbose term "type class" to refer to Haskell classes, and to avoid confusion with concepts from other languages.
  getExactlyOne :: t -> ExactlyOne a
  --                    ^^^^^^^^^^^^ type of return value
  --               ^ type of first argument
  -- ^^^^^^^^^^ first associated function (a.k.a. method) of `HasExactlyOne`
  --            even though the word "method" is used, these are not methods of an object. they are global functions.
  --            there is no actual definition, because each instance will define its own implementation.

  withExactlyOne :: (ExactlyOne a -> ExactlyOne a) -> t -> t
  --                                                       ^ type of return value
  --                                                  ^ type of second argument
  --                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type of first argument
  -- ^^^^^^^^^^^ second method `HasExactlyOne`

instance HasExactlyOne (ExactlyOne a) a where
--                                    ^ value for second type variable
--                     ^^^^^^^^^^^^^^ value for first type variable
--       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ instance head, expresses the fact that 'HasExactlyOne t a' is true for 't' = 'ExactlyOne a' and 'a' = 'a'
--                                      This instance gives us facts of the form `HasExactlyOne (ExactlyOne Int) Int` and similar.
-- ^^^^^ as with the term "class," empty yourself of all of your preconceived notions about what the word "instance" means.
--       Whereas the class defines a predicate, a property that may or may not be true about particular types,
--       an instance is a particular fact about some particular types.
--       Think of the class as a template for a question.
--       Instance are an answer (always in the affirmative) to the question posed by the class.
--       This isntance expresses the fact that `HasExactlyOne (ExactlyOne a) a` is true.
--       For any combination of type variables, there can only ever be zero or one instance of a given class.
--       In other words, our code will fail to compile if we ever try to introduce another `instance HasExactlyOne (ExactlyOne a) a` declaration somewhere else.
--       An instance for some given class and some particular types is unique, if it exists at all.
  getExactlyOne :: ExactlyOne a -> ExactlyOne a
  --               ^^^^^^^^^^^^ 't', the first type variable in the class definition, is instantiated to 'ExactlyOne a' in this instance
  --               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ signature of 'getExactlyOne'
  getExactlyOne = id
  --              ^^ the /identity/ function, which returns its argument unchanged
  --           ^ no arguments, so right-hand side need be a function of type 'ExactlyOne a -> ExactlyOne a'

  withExactlyOne :: (ExactlyOne a -> ExactlyOne a) -> ExactlyOne a -> ExactlyOne a
  --                                                                  ^^^^^^^^^^^^ ditto. in this instance, 't' is 'ExactlyOne a'
  --                                                  ^^^^^^^^^^^^ 't' is instantiated to 'ExactlyOne a' in this instance
  withExactlyOne = id
  --               ^^ again, the identity function

-- | Question ExactlyOne.2
--
--     a) How is it okay that `getExactlyOne` and `withExactlyOne` are defined without reference to any arguments?
--     b) How does your answer to _Part a_ relate to the concept that functions are first-class values in Haskell?
question_ExactlyOne_2 :: (P.String, P.String)
question_ExactlyOne_2 =
  ( error "todo"
  , error "todo"
  )

instance HasExactlyOne (ExactlyOne a, b) a where
--       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ instance head, expresses the fact that `HasExactlyOne t a` is true for `t` = `(ExactlyOne a, b)` and `a` = `a`
--                                         This instance gives us facts like `HasExactlyOne (ExactlyOne Double, String) Double` and similar.
  getExactlyOne :: (ExactlyOne a, b) -> ExactlyOne a
  --               ^^^^^^^^^^^^^^^^^ `t` is instantiated to `(ExactlyOne a, b)` in this instance
  getExactlyOne (ExactlyOne a0, _) = ExactlyOne a0

  withExactlyOne :: (ExactlyOne a -> ExactlyOne a) -> (ExactlyOne a, b) -> (ExactlyOne a, b)
  --                                                  ^^^^^^^^^^^^^^^^^ `t` instantiated to `(ExactlyOne a, b)`
  withExactlyOne f (ExactlyOne a0, b0) = (f (ExactlyOne a0), b0)

instance HasExactlyOne t a => HasExactlyOne (b, t) a where
--                            ^^^^^^^^^^^^^^^^^^^^^^ instance head.
--                                                   Whatever types you choose for `b`, `t`, and `a`, `HasExactlyOne (b, t) a` will be true so long as `HasExactlyOne t a` is true.
--       ^^^^^^^^^^^^^^^^^ instance context. this is a precondition that must be true in order for this instance to be true
  getExactlyOne :: (b, t) -> ExactlyOne a
  --               ^^^^^^ `t` instantiated to `(b, t)`
  getExactlyOne (_, t0) = getExactlyOne t0
  --                      ^^^^^^^^^^^^^ apply `getExactlyOne` at type `t -> ExactlyOne a`. this is why we need the context `HasExactlyOne t a`
  --             ^ we don't need the first component, so don't even assign it a name
  --            ^^^^^^^ pattern match the pair

  withExactlyOne :: (ExactlyOne a -> ExactlyOne a) -> (b, t) -> (b, t)
  withExactlyOne f (b0, t0) = (b0, withExactlyOne f t0)
  --                               ^^^^^^^^^^^^^^ apply `withExactlyOne` at type `(ExactlyOne a -> ExactlyOne a) -> t -> t`
  --                          ^^^^^^^^^^^^^^^^^^^^^^^^^ pack things back up into a pair
  --               ^^^^^^^^ pattern match the pair

overwriteExactlyOne :: HasExactlyOne t a => ExactlyOne a -> t -> t
--                                                               ^ type of return value
--                                                          ^ type of second argument
--                                          ^^^^^^^^^^^^ type of first argument
--                                     ^ constrained type variable
--                                   ^ constrainted type variable. this function is _constrained polymorphic_
--                     ^^^^^^^^^^^^^^^^^ function constraint. 'HasExactlyOne t a' must be true to use this function
overwriteExactlyOne ea t = withExactlyOne (\_ -> ea) t
--                                               ^^ body of our lambda expression. we ignore whatever `ExactlyOne a` value was there, and we replace it with `ea`
--                                          ^ first (and only) argument of our lambda expression. we don't use it, so we don't assign it a name
--                                         ^^^^^^^^ lambda expression, i.e. a function literal
--                         ^^^^^^^^^^^^^^ apply `withExactlyOne` at type `(ExactlyOne a -> ExactlyOne a) -> t -> t`. this is why we need the constraint
--                     ^ second argument. we named it after its type, `t`.
--                  ^^ first argument

instance Show a => Show (ExactlyOne a) where
--                 ^^^^^^^^^^^^^^^^^^^ instance head. `Show (ExactlyOne a)` is true so long as `Show a` is true
--       ^^^^^^ instance context. we're going to need `Show a` to be true in order to implement `Show (ExactlyOne a)`
--       ^^^^ a type class for types that have a string representation
  show :: ExactlyOne a -> P.String
  show (ExactlyOne x) = "ExactlyOne(" P.++ show x P.++ ")"

-- | Question ExactlyOne.3
--
-- Remove the instance context so that it reads `instance Show (ExactlyOne a) where`.
--  a) What does the compiler tell you when you try to compile it?
--  b) How does the compiler come to its conclusion?
question_ExactlyOne_3 :: (P.String, P.String)
question_ExactlyOne_3 =
  ( error "todo"
  , error "todo"
  )

showAndTell :: forall t a. (HasExactlyOne t a, Show a) => t -> P.String
--                                             ^^^^^^ `Show a` must be true to use `showAndTell`
--                          ^^^^^^^^^^^^^^^^^ `HasExactlyOne t a` must be true to use `showAndTell`
--                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^ constraints. `showAndTell` is constrained polymorphic
--                      ^ declared type variable `a`
--                    ^ declared type variable `t`
--             ^^^^^^ declare type variables. usually optional, but we refer to `a` and `t` in the function body, so we need this declaration
showAndTell t0 =
  let
-- ^^ keyword that allows us to make local definitions. begins a block, so indent
    ea :: ExactlyOne a
    -- ^^ type signature. usually optional, but we need this one in order to used `getExactlyOne`
    ea = getExactlyOne t0
   in show ea
   -- ^^^^ `show` is constrained polymorphic. It's least-specific signature is `show :: forall t. Show t => t -> String`
   --      `show`s argument, `ea`, has type `ExactlyOne a`, so we are applying `show` with `t` = `ExactlyOne a`
   --      the compiler tries to determine if `Show (ExactlyOne a)` is true
   --      It finds
-- ^^ keyword that says we're done making definitions

-- | Question ExactlyOne.4
--
-- Remove the type signature `ea :: ExactlyOne a` from `showAndTell`.
--  a) What does the compiler tell you when you try to compile it?
--  b) Why is the compiler unable to determine the type of `ea`?
question_ExactlyOne_4 :: (P.String, P.String)
question_ExactlyOne_4 =
  ( error "todo"
  , error "todo"
  )

-- $noteToTrainee
-- The rest of the code is only needed for the test suite. Move on to [Course.Validation](../Validation.hs)

instance P.Functor ExactlyOne where
    fmap = M.liftM

instance A.Applicative ExactlyOne where
    (<*>) = M.ap
    pure = ExactlyOne

instance P.Monad ExactlyOne where
    (>>=) = flip bindExactlyOne
