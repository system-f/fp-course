{- sent by user napping via IRC 20110629 licence unknown -}

module tictactoe where
open import Data.Bool hiding (_≟_)
open import Data.Vec
open import Data.Fin
open import Data.Nat
open import Data.Maybe
open import Category.Monad
open import Relation.Nullary.Core
open import Data.Product hiding (map)

data Cell : Set where
  X O b : Cell
cell-eqb : Cell → Cell → Bool
cell-eqb X X = true
cell-eqb O O = true
cell-eqb b b = true
cell-eqb _ _ = false

Board = Vec Cell 9

startBoard : Board
startBoard = tabulate (λ _ → b)

triple-winner : Cell → Cell → Cell → Maybe Cell
triple-winner X X X = just X
triple-winner O O O = just O
triple-winner _ _ _ = nothing

winner : Board → Maybe Cell
winner (ul ∷ uc ∷ ur ∷ ml ∷ mc ∷ mr ∷ bl ∷ bc ∷ br ∷ nil) =
   triple-winner ul uc ur ∣ triple-winner ml mc mr ∣ triple-winner bl bc br
 ∣ triple-winner ul ml bl ∣ triple-winner uc mc bc ∣ triple-winner ur mr br
 ∣ triple-winner ul mc br ∣ triple-winner ur mc bl
 where open RawMonadPlus monadPlus

next-player : Board → Cell
next-player board with sum (map Xs board) | sum (map Os board)
  where Xs : Cell → ℕ
        Xs X = 1
        Xs _ = 0
        Os : Cell → ℕ
        Os O = 1
        Os _ = 0
next-player board | xs | os with xs ≟ os | xs ≟ suc os
next-player board | xs | os | yes _ | _ = X
next-player board | xs | os | _ | yes _ = O
next-player board | xs | os | _ | _ = b

validMove : Fin 9 → Cell → Board → Bool
validMove p v board = cell-eqb b (lookup p board)
  ∧ maybe′ (λ _ → false) true (winner board)
  ∧ cell-eqb v (next-player board)

data Game : Board → Set where
  startGame : Game startBoard
  move : (pos : Fin 9) → (val : Cell) → 
    ∀ {board} → {ev : T (validMove pos val board)} → Game board
    → Game (board [ pos ]≔ val)

started : Board → Bool
started = foldr _ blank false
  where blank : Cell → Bool → Bool
        blank b f = f
        blank _ _ = true


prevBoard : ∀ {board} → {ev : T (started board)} → (g : Game board) → Board
prevBoard {ev = ()} startGame
prevBoard (move pos val {board} y) = board

takeBack : ∀ {board} → {ev : T (started board)} → (g : Game board) →
  Game (prevBoard {ev = ev} g)
takeBack {ev = ()} startGame
takeBack (move pos val {board} y) = y

getBoard : ∀ {board} → Game board → Board
getBoard {board} _ = board

-- example

state0 = startGame
-- state0' = takeBack state0 -- not started
state1 = move (# 4) X state0
state2 = move (# 1) O state1
-- state2' = move (# 0) X state1 -- out of turn
state3 = takeBack state2
state4 = move (# 0) O state3
state5 = move (# 2) X state4
-- state5' = move (# 0) X state4 -- already occupied
state6 = move (# 6) O state5
state7 = move (# 5) X state6
state8 = move (# 3) O state7
-- state9 = move (# 8) X state8 -- already finished

