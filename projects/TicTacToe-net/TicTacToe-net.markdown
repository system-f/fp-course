TicTacToe Network Server
========================

Using the TicTacToe project, produce a multi-user network server for the game
tic-tac-toe. For simplicity, use `forkIO` and `IORef` structures for concurrent
access. Game players will not be managed, so any user may be able to manipulate
the game board at any time.

Protocol
--------

The game server will maintain a single game of tic-tac-toe and a history of
completed games. Clients send line-terminated instructions to the game server.

Instructions begin with a case-insensitive command:

* MOVE <position>

  Make a move on the current game board at the given position.

* GAME

  View the current game board.

* FINISHED

  View completed games.

* CHAT <message>

  Send a message to all connected tic-tac-toe players.

* TURN

  View whose turn it is to play on the current game board.

* AT <position>

  View which player is at the given position.

The position may be a digit [1-9] indicating a numeric position, or it may be a
case-insensitive cardinal direction.

    1 2 3
    4 5 6
    7 8 9

    NW   N  NE
    W    C   E
    SW   S  SE

Making a move at a position on a game board that has gone out of date with what
that connected client believes to be the current game state fails. A client may
update what they believe to be the game state with the `GAME` command.

Getting Started
---------------

Some library code has been written for you. Some has been specified with types
and some will need to be written from scratch. Use the wide array of tools
available to achieve this.

Start by creating the `.cabal` file for your project. You will need to specify
an `executable` section in the file for the server program.

The final goal is to complete the `server` and `game` functions, so that the
`main` function will execute.

Game
----

Of note is the `Game` data type.

    newtype Game f a =
      Game (Env -> f (a, Unfinished, FinishedGames))

This is a monad stack of reader (`(->) Env`) and state, that also includes an
arbitrary monad on top (`f`). The inclusion of an arbitrary monad with the
existing stack makes `Game` a _monad transformer_.

A significant part of this exercise is to build library components that combine
`Game` values to produce new values. For example, consider a `Game` value that
might read and produce the `Accept` value from the environment `Env`.

Such a function can be provided for any monad (`f`) on the stack. Notice the
current game state (`getUnfinished env`) and finished games (`getFinished env`)
are read from the environment and then returned unchanged.

    accept ::
      Game f Accept
    accept =
      Game $ \env -> return (getAccept env, getUnfinished env, getFinished env)

As another example, consider a `Game` value that prints its environment, then
removes all finished games. Such a function can only operate in `IO` over the
existing stack:

    printAndClear ::
      Game IO ()
    printAndClear =
      Game $ \env -> print env >> return ((), getUnfinished env, [])

As part of this exercise, you will be thinking about which values you need to
achieve the requirement and creating them as they are needed.
