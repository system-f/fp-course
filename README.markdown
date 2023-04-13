# Functional Programming Course

![System-F](https://system-f.gitlab.io/logo/systemf-450x450.jpg)

### Written by Tony Morris & Mark Hibberd

### With contributions from individuals and organisations (thanks!)

#### Note to Mercury Engineers

Getting started is easy.
All you need are `ghc`, `stack`, and `ghcid`.
Setup instructions [here](https://github.com/MercuryTechnologies/haskell-curriculum/blob/main/guides/mentees/toolchain-setup.md).

To compile and run tests when a file changes, use this command (from the project root directory):

```
ghcid
```

#### Introduction

The course is structured according to a linear progression and uses the Haskell
programming language to learn programming concepts pertaining to functional
programming.

Exercises are annotated with a comment containing the word "Exercise." The existing code compiles, however answers have
been replaced with a call to the Haskell `error` function and so the code will throw an exception if it is run. Some
exercises contain tips, which are annotated with a preceding "Tip:". It is not necessary to adhere to tips. Tips are
provided for potential guidance, which may be discarded if you prefer a different path to a solution.

The exercises are designed in a way that requires personal guidance, so if you
attempt it on your own and feel a little lost, this is normal. All the
instructions are not contained herein.

### Getting Started

1. Install the Glasgow Haskell Compiler (GHC) version 8.0 or higher.
   [GHCup](https://www.haskell.org/ghcup/) is the recommended way to do that these days

2. Change to the directory containing this document.

3. Execute the command `ghci`, which will compile and load all the source code.
   You may need to set permissions on the root directory and the ghci configuration
   file, `chmod go-w .ghci ./`.

4. Inspect the introductory modules to get a feel for Haskell's syntax, then move
   on to the exercises starting with `Course.Optional`. The
   [Progression](#progression) section of this document lists the recommended
   order in which to attempt the exercises.

5. Edit a source file to a proposed solution to an exercise. At the `ghci`
   prompt, issue the command `:reload`. This will compile your solution and
   reload it in the GHC interpreter. You may use `:r` for short.

### Tips after having started

1. Some questions take a particular form. These are called *WTF questions*. WTF
   questions are those of this form or similar:
  * What does ____ mean?
  * What does the ____ function mean?
  * What is a ____ ?
  * Where did ____ come from ?
  * What is the structure of ____ ?

  They are all answerable with the `:info` command. For example, suppose you
  have the question, "What does the `swiggletwoop` function mean?" You may
  answer this at GHCi with:

  `> :info swiggletwoop`

  You may also use `:i` for short.

2. Functional Programming techniques rely heavily on types. This reliance may
   feel foreign at first, however, it is an important part of this course. If
   you wish to know the type of an expression or value, use `:type`. For
   example,

   `> :type reverse`

   `List t -> List t`

   This tells you that the `reverse` function takes a list of elements of some
   arbitrary type (`t`) and returns a list of elements of that same type. Try
   it.

   You may also use `:t` for short.

3. GHCi has TAB-completion. For example you might type the following:

   `> :type rev`

   Now hit the TAB key. If there is only one function in scope that begins with
   the characters `rev`, then that name will auto-complete. Try it. This
   completion is context-sensitive. For example, it doesn't make sense to ask
   for the type of a data type itself, so data type names will not auto-complete
   in that context, however, if you ask for `:info`, then they are included in
   that context. Be aware of this when you use auto-complete.

   This also works for file names:

   `> readFile "/etc/pas"`

   Now hit the TAB key. If there is only one existing filename on a path that
   begins with `/etc/pas`, then that name will auto-complete. Try it.

   If there is more than one identifier that can complete, hit TAB twice
   quickly. This will present you with your options to complete.

4. Follow the types.

   You may find yourself in a position of being unsure how to proceed for a
   given exercise. You are encouraged to adopt a different perspective. Instead
   of asking how to proceed, ask how you might proceed while adhering to the
   guideline provided by the types for the exercise at hand.

   It is possible to follow the types without achieving the desired goal,
   however, this is reasonably unlikely at the start. As you become more reliant
   on following the types, you will develop more trust in the potential paths
   that they can take you, including identification of false paths.

   Where types fall short, use the tests written in comments above each exercise.
   They can be copied and pasted into GHCi. You should also take the first step
   of following the types. Do it.

5. Do not use tab characters

   Set up your text editor to use space characters rather than tabs.
   Using tab characters in Haskell can lead to confusing error messages.
   GHC will give you a warning if your program contains a tab character.

### Progression

We recommend you perform some exercises before others. The first step
is to inspect the introduction modules.

* `Course.ExactlyOne`
* `Course.Validation`

They contain examples of data structures and Haskell syntax. They do not contain
exercises and exist to provide a cursory examination of Haskell syntax. The next
step is to complete the exercises in `Course.Optional`.

After this, we recommend the following progression of modules:

* `Course.List`
* `Course.Functor`
* `Course.Applicative`
* `Course.Monad`
* `Course.FileIO`
* `Course.State`
* `Course.Compose`
* `Course.StateT`
* `Course.Parser` *(see also `Course.Person` for the parsing rules)*
* `Course.MoreParser`
* `Course.JsonParser`

Answers for the exercises can be found here:
[https://github.com/tonymorris/fp-course](https://github.com/tonymorris/fp-course)

After these are completed, complete the exercises in the `projects` directory.

### Introducing Haskell

This section is a guide for the instructor to introduce Haskell syntax. Each of
these points should be covered before attempting the exercises.

* values, assignment
* type signatures `::` reads as *has the type*
  * The `->` in a type signature is *right-associative*
* functions are values
* functions take arguments
  * functions take *only one argument* but we approximate with spoken
    language
  * functions can be declared inline using *lambda expressions*
  * the `\` symbol in a lambda expression denotes a Greek lambda
* operators, beginning with non-alpha character, are in infix position by
  default
  * use in prefix position by surrounding with *(parentheses)*
* regular identifiers, beginning with alpha character, are in prefix position by
  default
  * use in infix position by surrounding with ``backticks``
* polymorphism
  * type variables *always* start with a lower-case character
* data types, declared using the `data` keyword
  * following the `data` keyword is the *data type name*
  * following the data type name are zero of more type variables
  * then `=` sign
  * data types have zero or more constructors
    * data type constructors start with an upper-case character, or colon `(:)`
  * following each constructor is a list of zero or more *constructor arguments*
  * between each constructor is a pipe symbol `(|)`
  * the `deriving` keyword gives us default implementations for some functions
    on that data type
  * when constructors appear on the left side of `=` we are *pattern-matching*
  * when constructors appear on the right side of `=` we are *constructing*
* type-classes

### Learning the tools

When this course is run in-person, some tools, particularly within Haskell, are
covered first.

* GHCi
  * `:type`
  * `:info`
* values
* type signatures
  * `x :: T` is read as *x is of the type T*
* functions are values
* functions take arguments
* functions take one argument
* lambda expressions
* operators (infix/prefix)
  * identifiers starting with `isAlpha` are prefix by default, infix surrounded in backticks (\`)
  * other identifiers are infix by default, prefix surrounded in parentheses
* data types
  * `data` keyword
  * recursive data types
* pattern matching
* `deriving` keyword
* type-classes
* type parameters
  * always lower-case 'a'..'z'
  * aka generics, templates C++, parametric polymorphism

### Parser grammar assistance

The exercises in `Parser.hs` can be assisted by stating problems in a specific way, with a conversion to code.

| English   | Parser library       |
| --------- | -------------------- |
| and then  | `bindParser` `>>=`   |
| always    | `valueParser` `pure` |
| or        | `\|\|\|`             |
| 0 or many | `list`               |
| 1 or many | `list1`              |
| is        | `is`                 |
| exactly n | `thisMany n`         |
| call it x | `\x ->`              |

### Monad comprehension (a.k.a. do-notation)

* insert the word `do`
* turn `>>=` into `<-`
* delete `->`
* delete `\`
* swap each side of `<-`

### References

* [The Haskell `error` function](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:error)

* [Glasgow Haskell Compiler](http://haskell.org/ghc)
