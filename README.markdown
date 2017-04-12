# Functional Programming Course

![Data61](http://i.imgur.com/0h9dFhl.png)

### Written by Tony Morris & Mark Hibberd for Data61 (formerly NICTA)

### With contributions from individuals (thanks!)

#### Special note 1

If you have arrived here by https://github.com/tonymorris/fp-course and you are
looking for the *exercises* (not the answers), please go to
https://github.com/data61/fp-course

#### Special note 2

Since February 2017, this repository is no longer hosted at
https://github.com/NICTA/course which is deprecated. Data61 replaces what was 
NICTA since July 2016. The new repository is located at
https://github.com/data61/fp-course

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

### Getting Help

There are two mailing lists for asking questions. All questions are welcome,
however, your first post might be moderated. This is simply to prevent spam.

1. [[nicta-fp]](https://groups.google.com/forum/#!forum/nicta-fp) is a Google
   Group for any queries related to functional programming. This mailing list is
   owned by Data61 and is open to the public. Questions relating to this course
   are most welcome here.

2. [[haskell-exercises]](https://groups.google.com/forum/#!forum/haskell-exercises)
   is a Google Group for queries related specifically to this Data61 functional
   programming course material. This mailing list is not owned by Data61, but is
   run by others who are keen to share ideas relating to the course. 

3. \#scalaz [on Freenode](irc://irc.freenode.net/#scalaz) is an IRC channel that is operated
   by others who are keen to share ideas relating to functional programming in
   general. Most of the participants of this channel have completed the Data61 
   functional programming course to some extent. They are in various timezones
   and share a passion for functional programming, so may be able to provide
   relatively quick assistance with questions.

4. \#nicta-course [on Freenode](irc://irc.freenode.net/#nicta-course) is an IRC channel that
   is operated by others who are going through this course material on their
   own time and effort.

### Getting Started

1. Install the Glasgow Haskell Compiler (GHC) version 7.6 or higher.

2. Change to the directory containing this document.

3. Execute the command `ghci`, which will compile and load all the source code.
   You may need to set permissions on the root directory and the ghci configuration
   file, `chmod 600 .ghci ./`.

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

   Your instructor must guide you where types fall short, but you should also
   take the first step. Do it.

#### Running the tests

Some exercises include examples and properties, which appear in a comment above
the code for that exercise. Examples begin with `>>>` while properties begin
with `prop>`.

The solution to the exercise must satisfy these tests. You can check if you have
satisfied all tests with cabal-install and doctest. From the base directory of
this source code:

    > cabal update
    > cabal install cabal-install
    > cabal install --only-dependencies
    > cabal configure --enable-tests
    > cabal build
    > cabal test

Alternatively, you may run the tests in a single source file by using `doctest`
explicitly. From the base directory of this source code:

    > doctest -isrc -Wall -fno-warn-type-defaults <filename.hs>

Note: There is a [bug in GHC 7.4.1](http://ghc.haskell.org/trac/ghc/ticket/5820)
where for some configurations, running the tests will cause an unjustified
compiler error.

### Progression

It is recommended to perform some exercises before others. The first step is to
inspect the introduction modules.

* `Course.Id`
* `Course.Validation`

They contain examples of data structures and Haskell syntax. They do not contain
exercises and exist to provide a cursory examination of Haskell syntax. The next
step is to complete the exercises in `Course.Optional`.

After this, the following progression of modules is recommended:

* `Course.List`
* `Course.Functor`
* `Course.Applicative`
* `Course.Monad` (please see [this issue](https://github.com/NICTA/course/issues/118))
* `Course.FileIO`
* `Course.State`
* `Course.StateT`
* `Course.Extend`
* `Course.Comonad`
* `Course.Compose`
* `Course.Traversable`
* `Course.ListZipper`
* `Course.Parser` *(see also `Course.Person` for the parsing rules)*
* `Course.MoreParser`
* `Course.JsonParser`
* `Course.Interactive`
* `Course.Anagrams`
* `Course.FastAnagrams`
* `Course.Cheque`

During this progression, it is often the case that some exercises are abandoned
due to time constraints and the benefit of completing some exercises over
others. For example, in the progression, `Course.Functor` to `Course.Monad`, the
exercises repeat a similar theme. Instead, a participant may wish to do
different exercises, such as `Course.Parser`. In this case, the remaining
answers are filled out, so that progress on to `Course.Parser` can begin
(which depends on correct answers up to `Course.Monad`). It is recommended to 
take this deviation if it is felt that there is more reward in doing so.

Answers for the exercises can be found here:
[https://github.com/tonymorris/fp-course](https://github.com/tonymorris/fp-course)

After these are completed, complete the exercises in the `projects` directory.

### Leksah

If you choose to use the [Leksah IDE for Haskell](http://leksah.org/), the
following tips are recommended:

* Clone the git repo use Package -> Add to add course.cabal.
* Click on the green tick on the toolbar to include `cabal test`
  in each build and list the failures in the Errors pane.
* Choose Package -> Configure to make sure `--enable-tests`
  is used (just building may cause cabal to configure without).
* Ctrl + B to build (Command + B on OS X).
* The test failures should show up in Panes -> Errors.
* Pane -> Log often has useful error messages.
* Ctrl + J (Command + J on OS X) selects the next item in
  Errors pane and goes to it in the source (hold down Shift
  to go to previous item).
* Ctrl + Enter on a line starting "-- >>>" will run the
  selected expression in GHCi (Ctrl + Enter on OS X too).
  The output goes to Panes -> Log and Panes -> Output.
* The last GHCi expression is reevaluated after each :reload
  triggered by changes in the code.
* Uncheck Debug -> GHCi when you are done with GHCi and
  Leksah will go back to running cabal build and cabal test
  instead.

### Introducing Haskell

This section is a guide for the instructor to introduce Haskell syntax. Each of
these points should be covered before attempting the exercises.

* values, assignment
* type signatures `::` reads as *has the type*
  * The `->` in a type signature is *right-associative*
* functions are values
* functions take arguments
  * functions take *only one argument* but we approximate without spoken
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
* running the tests
  * `doctest`

### One-day

Sometimes this course material is condensed into one-day. In these cases, the
following exercises are recommended:

* `Optional`
  * `mapOptional`
  * `bindOptional`
  * `(??)`
  * `(<+>)`
* `List`
  * `headOr`
  * `product`
  * `length`
  * `map`
  * `filter`
  * `(++)`
  * `flatMap`
  * `reverse`
* `Functor`
  * `instance Functor List`
  * `instance Functor Optional`
  * `instance Functor ((->) t)`
  * `instance Functor void`
* `Applicative`
  * `instance Applicative List`
  * `instance Applicative Optional`
  * `instance Applicative ((->) t)`
  * `lift2`
  * `sequence`
* `FileIO`

### References

* [The Haskell `error` function](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:error)

* [Glasgow Haskell Compiler](http://haskell.org/ghc)
