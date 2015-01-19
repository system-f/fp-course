module TicTacType

%default total

%logging 0

-- TODO get rid of this helper

even :  Nat -> Bool
even Z = True
even (S Z) = False
even (S n) = not (even n)

-- TODO split Board into Board and ValidBoard, or make it abstract?

-----------------------------------------------------------------------
-- TIC TAC TYPE
-----------------------------------------------------------------------

{- This program represents a game of Tic Tac Type in Idris, really
   it should be called Naughts and Crosses, but we shall let this
   language attrocity slide in the name of a good pun. -}

-----------------------------------------------------------------------
-- PLAYERS
-----------------------------------------------------------------------

{- There are two players, X and O, who each play in turn. -}

data Player = X | O

instance Show Player where
  show X = "x"
  show O = "o"

instance Eq Player where
  X == X = True
  O == O = True
  X == O = False
  O == X = False


-----------------------------------------------------------------------
-- CELLS
-----------------------------------------------------------------------

{- A cell is a position in the game. A cell may be either occupied by a
   player or unoccupied -}


data Cell = Occupied Player | Unoccupied

instance Show Cell where
  show (Occupied p) = show p
  show Unoccupied     = "_"

instance Eq Cell where
  (Occupied a) == (Occupied b) = a == b
  (Occupied _) == Unoccupied = False
  Unoccupied == (Occupied _) = False
  Unoccupied == Unoccupied = True


-----------------------------------------------------------------------
-- POSITIONS
-----------------------------------------------------------------------

{- In our default 3x3 game, positions are the finite set of naturals less
   than 9, which index onto the board from left to right, top to bottom.
   i.e.

     0 | 1 | 2
    -----------
     3 | 4 | 5
    -----------
     6 | 7 | 8

 -}


Position : Type
Position = Fin 9

{- But given that this is kind of confusing way to deal with identifying
   a position, we also have these convenience definitions that let us
   treat a co-ordinate as a literal position.
   i.e.

     nw | n | ne
    -------------
     w  | c | e
    -------------
     sw | s | se

 -}


{- North-West, or the Top-Left corner position. -}
nw : Position
nw = 0

{- North, or the Top-Center position. -}
n : Position
n = 1

{- North-East, or the Top-Right corner position. -}
ne : Position
ne = 2

{- West, or the Left-Center position. -}
w : Position
w = 3

{- Center position. -}
c : Position
c = 4

{- East, or the Right-Center position. -}
e : Position
e = 5

{- South-West, or the Bottom-Left corner position. -}
sw : Position
sw = 6

{- South, or the Bottom-Center position. -}
s : Position
s = 7

{- South-East, or the Bottom-Right corner position. -}
se : Position
se = 8


-----------------------------------------------------------------------
-- THE BOARD
-----------------------------------------------------------------------

{- The board is a 3x3 grid represented as a Vector of length 9. This
   limits us to the default size, however it could easily be extended
   with limited effect on the rest of the program (excepting the algorithm
   that determines a win). -}

data Board = B (Vect 9 Cell)

instance Eq Board where
  (B a) == (B b) = a == b

instance Show Board where
  show (B [nw, n,  ne,
           w,  c,  e,
           sw, s,  se]) =
     " " ++ show nw ++ " | " ++ show n  ++ " | " ++ show ne ++ "\n" ++
     "-----------\n" ++
     " " ++ show w  ++ " | " ++ show c  ++ " | " ++ show e  ++ "\n" ++
     "-----------\n" ++
     " " ++ show sw ++ " | " ++ show s  ++ " | " ++ show se ++ "\n"

{- Next, we will define a bunch of useful combinators for working with
   boards. Conveniently we get to define these at the value level, even
   though we will end up using them on the type level most of the time.  -}


-- Convert a board back to its vectorized form
toVect : Board -> Vect 9 Cell
toVect (B v) = v

-- An intial empty board
empty : Board
empty =
  B [Unoccupied, Unoccupied, Unoccupied,
     Unoccupied, Unoccupied, Unoccupied,
     Unoccupied, Unoccupied, Unoccupied]

-- Do we have 3 in a row?
match : Cell -> Cell -> Cell -> Maybe Player
match (Occupied a) (Occupied b) (Occupied c) = toMaybe (a == b && b == c) a
match _ _ _  = Nothing

-- Is there a winner on the board?
winner : Board -> Maybe Player
winner (B [nw, n,  ne,
           w,  c,  e,
           sw, s,  se]) =
  match ne n nw <|> match e  c w  <|> match se s sw <|> match ne e se <|>
  match n  c s  <|> match nw w sw <|> match ne c sw <|> match nw c se

-- Is this a valid board?
isValidBoard : Board -> Bool
isValidBoard board =
  let xs = sum . map xToInt . toVect $ board in
  let ys = sum . map oToInt . toVect $ board in
  xs == ys || xs == (S ys) where

  xToInt Unoccupied = 0
  xToInt (Occupied X) = 1
  xToInt (Occupied O) = 0

  oToInt Unoccupied = 0
  oToInt (Occupied X) = 0
  oToInt (Occupied O) = 1

-- How many positions are occupied ?
occupied : Board -> Nat
occupied board =
  sum . map toInt . toVect $ board where

  toInt Unoccupied = 0
  toInt (Occupied _) = 1

-- What is at the specified position ?
at : Position -> Board -> Cell
at position (B vect) = index position vect

-- Who's turn is it?
turn : Board -> Player
turn board =
  if even (occupied board) then X else O

-- Is this position free?
free : Position -> Board -> Bool
free position board =
  at position board == Unoccupied

-- Is this game complete?
complete : Board -> Bool
complete board =
  isJust (winner board) || occupied board == (length . toVect $ board)

-- Is this a valid move for the current board?
isValidMove : Position -> Player -> Board -> Bool
isValidMove position player board =
  free position board && (not . isJust) (winner board) && turn board == player

{- We want a data structure to carry the proof that a move is valid for a given
   board, so we can use it later on. This is more useful than the Bool above. -}

data ValidMove : Board -> Type where
  IsValidMove : Position -> Player -> (b : Board) -> ValidMove b

{- To construct these, we first build a function that "might" produce a ValidMove
   depending on the constraints of `isValidMove`. -}

tryValidMove : Position -> Player -> (b : Board) -> Maybe (ValidMove b)
tryValidMove position player board =
  toMaybe (isValidMove position player board) (IsValidMove position player board)

{- But the great thing about all these types, is that we can directly construct a
   valid move if we can prove it. -}

validMove : (position : Position) -> (player : Player) -> (board : Board) ->
            {default ItIsJust prf : (IsJust (tryValidMove position player board))} -> ValidMove board
validMove position player board {prf} with (tryValidMove position player board)
  validMove position player board {prf = ItIsJust} | Just y = y

-- Lets run a validate move and produce a new board.

runMove : ValidMove board -> Board
runMove (IsValidMove position player (B board)) =
  B $ replaceAt position (Occupied player) board


{- Now a similar treatment to create a safe board constructor. -}

tryBoard : Vect 9 Cell -> Maybe Board
tryBoard vect = let b = B vect in toMaybe (isValidBoard b) b

board : (vect: Vect 9 Cell) -> {default ItIsJust prf : (IsJust (tryBoard vect))} -> Board
board vect {prf} with (tryBoard vect)
  board vect {prf = ItIsJust} | Just y = y


-----------------------------------------------------------------------
-- THE GAME
-----------------------------------------------------------------------

{- Now we want to build a Game data type that will hold our game state.
   Importantly, we are now lifting the "Board" to the type level, which
   means that the type of a game carries around the entire board state. -}

data Game : Board -> Type where

  -- start a new game with an empty board
  start : Game empty

  -- start from some arbitrary starting point
  load : (b : Board) -> Game b

  -- make a (guaranteed to be valid) move on the current game
  move' : {b : Board} -> (m : ValidMove b) -> Game b -> Game (runMove m)

{- Now our "game" library -}

-- an alias for validMove which makes it easier to declare moves (go ne)
move : {board : Board} -> (position : Position) -> (player : Player) -> (game : Game board) ->
     {default ItIsJust prf : (IsJust (tryValidMove position player board))} -> Game (runMove $ validMove position player board {prf})
move {board} position player game {prf} = move' (validMove position player board {prf}) game

-- A data type to hold a proof that we have a previous game state available
data Prev : Game b -> Type where
  HasPrev : {bb : Board} -> {m : ValidMove bb} -> {g : Game bb} ->  Prev (move' m g)

-- Give me the board for the current game state.
toBoard : {b : Board} -> Game b -> Board
toBoard {b} _ = b

-- Has the game started?
hasStarted : {b : Board} -> Game b -> Bool
hasStarted {b} _ = occupied b > 0

-- What was the previous board? (only if you have a proof that there is a previous board)
previousBoard : (gg : Game b) -> {default HasPrev prf : (Prev gg)} -> Board
previousBoard (move' {b} m g) {prf = HasPrev} = b

-- Take back the last move? (only if you have a proof that there is a previous board)
takeBack : (gg : Game b) -> {default HasPrev prf : (Prev gg)} -> Game (previousBoard gg {prf})
takeBack (move' {b} m g) {prf = HasPrev} = g

-- Who if any holds the specified position?
playerAt : {b: Board} -> Position -> Game b -> Maybe Player
playerAt {b} p _ = case at p b of
  Occupied player => Just player
  Unoccupied => Nothing

-- Who won? (if there is final state)
whoWon : {b : Board} -> Game b -> {default oh prf : so (complete b) } -> Maybe Player
whoWon {b} _ = winner b


-----------------------------------------------------------------------
-- DEMO
-----------------------------------------------------------------------

state0 : ?state0t
state0 = start
state0t = proof search

--state0x : ?state0xt
--state0x = takeBack state0
--state0xt = proof search

state1 : ?state1t
state1 = move ne X state0
state1t = proof search

