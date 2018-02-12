## Cheatsheet

### Vocabulary

|**Symbol**|**Pronunciation**    |**Notes**                             |
|----------|---------------------|------------------------------------  |
|`:.`      |cons                 |Adds an element to the front of a list|
|`<$>`     |eff-map              |Member of the Functor type class      |
|`<*>`     |app, apply, spaceship|Member of the Applicative type class  |
|`>>=`     |bind                 |Member of the Monad type class        |


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

### Use `:info` to ask GHC questions

If you ever want to know what an identifier is, you can ask GHCi using `:info` or just `:i`.
For example, if you see `List` somewhere in your code, and want to know more about it, enter
`:i List` in GHCi.

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
