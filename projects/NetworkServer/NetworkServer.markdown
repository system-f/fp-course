Chat Network Server
===================

The goal of this exercise is to apply functional programming
constructs to a more complex problem. We shall explore the benefits of
functional programing in the large, embracing the presence of IO,
program state and related difficulties in our code.

Using the provided skeleton, the intention is to build a naive network
chat server, using a textual, line based protocol. This project provides
the required dependencies and structure to get a basic server up and
running.

Protocol
--------

Clients send line-terminated instructions to the game server.

Instructions begin with a case-insensitive command:

* CHAT <message>

  Send a message to all connected users.

* ADD <number>

  Add a number to the global counter and notify all connected users.

Server messages are sent prefixed by a '>' character.


Getting Started
---------------

Some library code has been written for you. Some has been specified with types
and some will need to be written from scratch. Use the wide array of tools
available to achieve this.

To run your server (although it will error out, until you have completed the
implementation):

```
cabal configure
cabal build
./dist/build/network-chat/network-chat
```

Start by taking a few minutes familiarising yourself with what is has
been provided. You should be able to implement the problem primarily
using functions provided in this project and those you have used to
complete earlier course exercises. But remember this is _not_ an
exercise to learn haskell APIs, so anything you think is missing or
don't understand, ask straight away.

Then you want to start by looking at the `error TODO` comments in
`Network.Server.Chat.Loop` and `Network.Server.Chat.Chat`.

Loop
----

Of note is the `Loop` data type.

    data Loop f a =
      Loop (Env v -> f a)

This is a monad stack of reader (`(->) Env v`), that also includes an
arbitrary monad on top (`f`). The inclusion of an arbitrary monad with the
existing stack makes `Loop` a _monad transformer_.

A significant part of this exercise is to build library components that combine
`Loop` values to produce new values. For example, consider a `Loop` value that
might read and produce the `Accept` value from the environment `Env`.

Such a function can be provided for any monad (`f`) on the stack. Notice the
current game state (`getUnfinished env`) and finished games (`getFinished env`)
are read from the environment and then returned unchanged.

    accept ::
      Loop f Accept
    accept =
      Loop $ \env -> return (getAccept env)

As part of this exercise, you will be thinking about which values you need to
achieve the requirement and creating them as they are needed.

Goals
-----

* Be able to have multiple telnet sessions connect to your server and
  communicate. For example

Session 1:
```
telnet localhost 6060
Connected to localhost.
Escape character is '^]'.
CHAT hello everyone
> h1
> counter is at 5
```

Server 2:
```
telnet localhost 6060
Connected to localhost.
Escape character is '^]'.
> hello everyone
CHAT hi
> counter is at 5
```

Server 3:
```
telnet localhost 6060
Connected to localhost.
Escape character is '^]'.
> hello everyone
> hi
ADD 5
> counter is at 5
```

* Maximise the amount of _pure_ code in your system. And, in turn minimise
  the amount of code tied to the Game transformer or even worse IO.

* Bonus: 0 `hlint` issues.



TicTacToe Network Server (Single Game)
======================================

NOTE: This assumes the chat server is completed.

Now that we have basic chat functionality, it is time to extend our network
server protocol to support a game of multi-user TicTacToe. There are a few
simplifying assumptions for this problem:

* Only one active game needs to be maintained by the server at a time.
* Game players will not be manages, so any user may be able to manipulate the board at any time.


```
cabal configure
cabal build
./dist/build/network-tictactoe/network-tictactoe
```

Protocol
--------

We will need to extend out the server state and protocol.
The game server will maintain a single game of tic-tac-toe and a history of
completed games.

Clients send line-terminated instructions to the game server.

Instructions begin with a case-insensitive command:

* MOVE <position>

  Make a move on the current game board at the given position.

* GAME

  View the current game board.

* FINISHED

  View completed games.

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

Since you have written the chat server already, you should be familiar with
most of the concepts required. To implement the TicTacToe part of the program
you should `import Data.TicTacToe` from the previous project.

You should expect to have to modify the `Game` monad transformer or its
related types to incorporate the new state required


TicTacToe Network Server (Multi-Game)
======================================

NOTE: This assumes the single game version of the TicTacToe server is completed.

Now that we have basic game functionality, it is time to extend our network
server protocol to support multiple concurrent games of multi-user TicTacToe.

At this point, you are on your own. You should now have the tools and techniques
required to discover and understand APIs based on their types.

HINT: A good place to start may be either <http://hackage.haskell.org/package/stm> which has a good write up at <http://book.realworldhaskell.org/read/software-transactional-memory.html> or <http://hackage.haskell.org/packages/archive/base/4.0.0.0/doc/html/Control-Concurrent-MVar.html> which has a good write up at <http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html>.
