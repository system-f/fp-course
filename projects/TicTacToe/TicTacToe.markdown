TicTacToe
=========

The goal is to write an API for the tic-tac-toe game. An API user, should be able to play a game of tic-tac-toe using this API, but importantly, it should be impossible for the API user to break the rules of the game. Specifically, if an attempt is made to break a rule, the API should reject the program. This is often done by way of a *compile-time type error*.

It is strongly advised that functional programming techniques are used to achieve the goal. This is because ensuring that the API adheres to the rules of tic-tac-toe, while rejecting a program otherwise, is difficult otherwise. No specific programming language is prescribed.

The following API functions should exist. By removing the need for some of these functions, the challenge becomes significantly easier. Removing some or all optional API functions is an advised path for someone who is looking to make the challenge easier.

* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is empty or in-play. Calling `move` on a game board that is finished is a *compile-time type error*.

*(optional)*  If fewer than 5 moves have been played, then this guarantees that the game is still in play, and so calling `move` will never produce a type-error in this case.

* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling `whoWon` on a game board that is empty or in-play is a *compile-time type error*. As an optional consideration, `whoWon` should never be a draw if fewer than nine moves have been played. In the case that the game is completed, but fewer than nine moves have been played, return a value that can only be one of two possibilities (the winner) and never a draw.

* `playerAt`: takes a tic-tac-toe board and position and returns the (possible) player at a given position. This function works on any type of board.

* `takeBack` *(optional)*: takes either a finished board or a board in-play that has had at least one move and returns a board in-play. It is a compile-time type error to call this function on an empty board.

* `isDraw` *(optional)* if called on a game with fewer than 9 moves, a compile-time type-error results.

* Other API functions that might be considered useful for general API use. Ensure that it is not possible to violate the game rules of tic-tac-toe. These functions can often be determined by also writing an interactive console application that uses the API -- other useful functions are likely to arise.

You should write automated tests for your API. For example, the following universally quantified property holds true:

`forall Board b. forall Position p. such that (not (positionIsOccupied
p b)). takeBack(move(p, b)) == b`

You should encode this property in an automated specification test. For Scala, use ScalaCheck. For Haskell, QuickCheck. For Java, consider [Functional Java](http://functionaljava.org/). For .NET, use [FsCheck](https://github.com/fsharp/FsCheck). For other languages, you may need to search around.

Haskell-specific
----------------

If you choose to use Haskell, also take advantage of its tooling:

* Build with CABAL
* Include a `.ghci` file for convenience when developing
  * https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci-dot-files.html
* API documented using Haddock
  * [http://www.haskell.org/haddock/doc/html/index.html](http://www.haskell.org/haddock/doc/html/index.html)
* Code style examined using hlint
  * `cabal install hlint`
  * Produce a report (`--report`)
  * [http://community.haskell.org/~ndm/darcs/hlint/hlint.htm](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm)
* Use hoogle and hayoo to find library functions
  * [http://haskell.org/hoogle/](http://haskell.org/hoogle/)
  * [http://holumbus.fh-wedel.de/hayoo/hayoo.html](http://holumbus.fh-wedel.de/hayoo/hayoo.html)


Extra-curricular
----------------
* Write an opponent that never loses
* Write an opponent with easy, medium, hard difficulty levels
