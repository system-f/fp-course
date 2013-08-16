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

