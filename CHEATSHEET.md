## Cheatsheet

### Vocabulary

|**Symbol**|**Pronunciation**    |**Notes**                                       |
|----------|---------------------|------------------------------------------------|
|`:.`      |cons                 |Adds an element to the front of a list          |
|`<$>`     |eff-map              |Member of the Functor type class                |
|`<*>`     |app, apply, spaceship|Member of the Applicative type class            |
|`>>=`     |bind                 |Member of the Monad type class                  |
|`bool`    |bool                 |if/then/else with arguments in the reverse order|

### Equivalent expressions

Here are some expressions and their neater, more idiomatic alternatives.

**Function application**

`\x -> f x` may be replaced with `f`

**Composition**

`\x -> f (g x)` may be replaced with `f . g`

### Follow the types

Rather than thinking operationally, focus on finding values for the types that the compiler is telling you you need.
Once you have a compiling program it's easier to look at what you have and decide if it solves your problem.

### Use type holes

Following on from the previous point, use type holes to discover the types of the values you need to
provide. A type hole is an underscore, or a name prefixed by an underscore (`_`). When GHC sees a
type hole, it will produce a compiler error that tells you the type of the value that should be in
its place.

As an example, let's assume we're attempting to write a definition for `List.product` using
`foldRight`, but we're not sure how to apply `foldRight` to get our solution. We can start by adding
some type holes.

```haskell
product ::
  List Int
  -> Int
product ns =
  foldRight _f _n ns
```

We can now reload the course code in GHCi and see what it tells us.

```
λ> :r
[ 5 of 26] Compiling Course.List      ( src/Course/List.hs, interpreted )

src/Course/List.hs:95:13: error:
    • Found hole: _f :: Int -> Int -> Int
      Or perhaps ‘_f’ is mis-spelled, or not in scope
    • In the first argument of ‘foldRight’, namely ‘_f’
      In the expression: foldRight _f _n ns
      In an equation for ‘product’: product ns = foldRight _f _n ns
    • Relevant bindings include
        ns :: List Int (bound at src/Course/List.hs:94:9)
        product :: List Int -> Int (bound at src/Course/List.hs:94:1)

src/Course/List.hs:95:16: error:
    • Found hole: _n :: Int
      Or perhaps ‘_n’ is mis-spelled, or not in scope
    • In the second argument of ‘foldRight’, namely ‘_n’
      In the expression: foldRight _f _n ns
      In an equation for ‘product’: product ns = foldRight _f _n ns
    • Relevant bindings include
        ns :: List Int (bound at src/Course/List.hs:94:9)
        product :: List Int -> Int (bound at src/Course/List.hs:94:1)
Failed, modules loaded: Course.Core, Course.ExactlyOne, Course.Optional, Course.Validation.
```

GHC is telling us a few helpful things here for each of our holes:

- The type of the hole: `Found hole: _f :: Int -> Int -> Int`
- Where it found the hole:
    ```
    In the first argument of ‘foldRight’, namely ‘_f’
    In the expression: foldRight _f _n ns
    In an equation for ‘product’: product ns = foldRight _f _n ns
    ```
- Bindings that are relevant to working out the type of the hole:
    ```
    Relevant bindings include
      ns :: List Int (bound at src/Course/List.hs:94:9)
      product :: List Int -> Int (bound at src/Course/List.hs:94:1)
    ```
    
Armed with this information we now have two smaller sub-problems to solve: choosing a function of
type `Int -> Int -> Int`, and choosing a value of type `Int`.

Keep in mind that this example is just for demonstrating the mechanics of type holes. The pay off
from deploying them increases as the difficulty and complexity of your problem increases, as they
allow you to break your problem into pieces while telling you the type of each piece.

### Use `:type` to ask GHC the type of expressions

If you've forgotten the type of an expression, or want to check if part of a solution type checks
and has the type that you expect, use `:type` or `:t` in GHCi.

```
λ> :t (:.)
(:.) :: t -> List t -> List t
λ> :t (:.) 5
(:.) 5 :: Num t => List t -> List t
λ> :t Nil
Nil :: List t
λ> :t (:.) 5 Nil
(:.) 5 Nil :: Num t => List t
λ> (:.) 5 Nil
[5]
```

### Use `:info` to ask GHC questions

If you ever want to know what an identifier is, you can ask GHCi using `:info` or just `:i`. For
example, if you see `List` somewhere in your code and want to know more about it, enter `:i List` in
GHCi. As shown below, it will print the constructors for values of that type, as well as the
instances for any type classes that are in scope.

```
λ> :i List
data List t = Nil | t :. (List t)
  	-- Defined at src/Course/List.hs:34:1
instance [safe] Eq t => Eq (List t)
  -- Defined at src/Course/List.hs:37:13
instance [safe] Ord t => Ord (List t)
  -- Defined at src/Course/List.hs:37:17
instance [safe] Show t => Show (List t)
  -- Defined at src/Course/List.hs:42:10
instance [safe] IsString (List Char)
  -- Defined at src/Course/List.hs:662:10
instance [safe] Functor List
  -- Defined at src/Course/Functor.hs:54:10
instance [safe] Extend List
  -- Defined at src/Course/Extend.hs:49:10
instance [safe] Applicative List
  -- Defined at src/Course/Applicative.hs:65:10
instance [safe] Monad List -- Defined at src/Course/Monad.hs:46:10
instance [safe] Traversable List
  -- Defined at src/Course/Traversable.hs:33:10
```

### Providing functions

If you're ever stuck providing a function as an argument or return value, insert a lambda with a
type hole. Continue this process recursively until you need to provide a simple value, then follow
the definitions back out.

Following on from our type holes example, if we're trying to solve `product` with `foldRight` and we
know the first argument to `foldRight` is a function, start by inserting the lambda.

```haskell
product ::
  List Int
  -> Int
product ns =
  foldRight (\a b -> _c) _n ns
```

After reloading this code, GHCi will tell us the type of `_c`, which in this case is `Int`. From the
previous type hole example, we know that both `a` and `b` are type `Int` (`_f :: Int -> Int ->
Int`), so it looks like we should do something with two `Int`s to produce an `Int`. A few operations
come to mind, but given we're defining `product`, let's go with multiplication.

```haskell
product ns =
  foldRight (\a b -> a * b) _n ns
```

It type checks. From here we'd need to pick an `Int` to replace `_n` and we'd have a solution that
at least type checks.

If `_c` had the type of another function, we'd simply insert another lambda in its place and
continue recursing. Alternatively, if `_c` had type `WhoosyWhatsits` and we didn't know anything
about that type or how to construct it, we could just ask GHCi using `:i WhoosyWhatsits` and
continue from there.

### Handling arguments

When you're not sure what to do with a function argument, try pattern matching it and looking at the
values that are brought into scope.

```haskell
data Bar = Bar Chars Int Chars

foo :: Bar -> Int
foo (Bar _ n _) = n
```

If your argument is a sum type that has multiple constructors, use `case` to pattern match and
handle each case individually.

```haskell
data Baz =
    C1 Int
  | C2 Chars Int

quux :: Baz -> Int
quux baz =
  case baz of
    C1 n   -> n
    C2 _ n -> n
```

You can also nest pattern matches as needed.

```haskell
data Thingo =
    X Int
  | Y (Optional Int)
    
f :: Thingo -> List Int
f t =
  case t of
    X n        -> n :. Nil
    Y (Full n) -> n :. Nil
    Y Empty    -> Nil
```

Finally, when you're not sure how to pattern match the argument because you don't know what its
constructors are, use `:info` as described above to find out.

```
λ> :i Baz
data Baz = C1 Int | C2 Chars Int
```
