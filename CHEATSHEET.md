## Cheatsheet

### Vocabulary

|**Symbol**|**Pronunciation**    |**Notes**                           |
|----------|---------------------|------------------------------------|
|`<$>`     |eff-map              |Member of the Functor type class    |
|`<*>`     |app, apply, spaceship|Member of the Applicative type class|


### Follow the types

Rather than thinking operationally, focus on finding values for the types that the compiler is telling you you need.
Once you have a compiling program it's easier to look at what you have and decide if it solves your problem.

### Use type holes

Following on from the previous point, use type holes to discover the types of the values you need to provide. A type
hole is an underscore, or a name prefixed by an underscore (`_`). When GHC sees a type hole, it will produce
a compiler error that tells you the type of the value that should be in its place.

```
λ> _f <$> Just "hello" <*> Just "world"

<interactive>:1:1: error:
    • Found hole: _f :: [Char] -> [Char] -> b
    ...
```

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
