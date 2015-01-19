import collection.immutable.{Map => M}

sealed trait Position {
  def toInt = this match {
    case NW => 1
    case N  => 2
    case NE => 3
    case W  => 4
    case C  => 5
    case E  => 6
    case SW => 7
    case S  => 8
    case SE => 9
  }

  def toChar: Char = (toInt + '0'.toInt).toChar
}
case object N extends Position
case object NE extends Position
case object E extends Position
case object SE extends Position
case object S extends Position
case object SW extends Position
case object W extends Position
case object NW extends Position
case object C extends Position

object Position {
  def positions = Set(N, NE, E, SE, S, SW, W, NW, C)

  def fromInt(n: Int) = n match {
    case 1 => Some(NW)
    case 2 => Some(N )
    case 3 => Some(NE)
    case 4 => Some(W )
    case 5 => Some(C )
    case 6 => Some(E )
    case 7 => Some(SW)
    case 8 => Some(S )
    case 9 => Some(SE)
    case _ => None
  }

  def fromChar(c: Char) = fromInt(c.toInt - 48)
}

sealed trait Player {
  def isPlayer1 = this == Player1
  def isPlayer2 = !isPlayer1

  def alternate = if(isPlayer1) Player2 else Player1

  def toSymbol = if(isPlayer1) 'X' else 'O'

  override def toString = if(isPlayer1) "Player 1" else "Player 2"
}
case object Player1 extends Player
case object Player2 extends Player

sealed trait GameResult {
  def isWin = this == Player1Wins || this == Player2Wins
  def isDraw = !isWin
  def winner = this match {
    case Player1Wins => Some(Player1)
    case Player2Wins => Some(Player2)
    case Draw        => None
  }

  override def toString = winner match {
    case Some(p) => p.toString + " wins"
    case None    => "draw"
  }
}
case object Player1Wins extends GameResult
case object Player2Wins extends GameResult
case object Draw extends GameResult

object GameResult {
  def win(p: Player) =
    p match {
      case Player1 => Player1Wins
      case Player2 => Player2Wins
    }
}

sealed trait EmptyBoard extends BoardLike {
  def -->(p: Position): Board = MapBoard(List((p, Player1)), Map((p, Player1)))

  def whoseTurn = Player1
  def isEmpty = true
  def nmoves = 0
  def occupiedPositions = Set.empty

  def playerAt(p: Position) = None

}
private case object EmptyBoardB extends EmptyBoard

object EmptyBoard {
  def empty: EmptyBoard = EmptyBoardB
}

trait BoardLike {
  def whoseTurn: Player
  def whoseNotTurn: Player = whoseTurn.alternate
  def isEmpty: Boolean
  def occupiedPositions: collection.Set[Position]
  def nmoves: Int
  def playerAt(p: Position): Option[Player]
  def playerAtOr(p: Position, pl: => Player) = playerAt(p) getOrElse pl
  def isOccupied(p: Position): Boolean = playerAt(p).isDefined
  def isNotOccupied(p: Position) = !isOccupied(p)
  def toString(f: (Option[Player], Position) => Char) = {    
    val z = ".===.===.===."
    def k(p: Position) = f(playerAt(p), p).toString

    List(
          z
        , List("| ", k(NW), " | ", k(N), " | ", k(NE), " |").mkString
        , z
        , List("| ", k( W), " | ", k(C), " | ", k( E), " |").mkString
        , z
        , List("| ", k(SW), " | ", k(S), " | ", k(SE), " |").mkString
        , z
        ) mkString "\n"   
  }
}

sealed trait TakenBack {
  def fold[X](isEmpty: => X, isBoard: Board => X): X
}

object TakenBack {
  def isEmpty: TakenBack = new TakenBack {
    def fold[X](isEmpty: => X, isBoard: Board => X) = isEmpty
  }

  def isBoard(b: Board): TakenBack = new TakenBack {
    def fold[X](isEmpty: => X, isBoard: Board => X) = isBoard(b)
  }
}

sealed trait Board extends BoardLike {
  private def moves = this match {
    case MapBoard(x, _) => x
  }

  private def map = this match {
    case MapBoard(_, x) => x
  }

  def takeBack = moves match {
    case Nil         => TakenBack.isEmpty
    case (p, _) :: t => TakenBack.isBoard(Board.board(t, map - p))
  }

  def isEmpty = false

  def occupiedPositions = map.keySet

  def nmoves = map.size

  def playerAt(p: Position) = map get p

  def whoseTurn = moves.head._2.alternate

  def -->(p: Position): MoveResult = {
    val j = map get p
    val mm = map + ((p, whoseTurn))
    val bb = Board.board((p, whoseTurn) :: moves, mm)
    val wins = List(
                     (NW, W , SW)
                   , (N , C , S )
                   , (NE, E , SE)
                   , (NW, N , NE)
                   , (W , C , E )
                   , (SW, S , SE)
                   , (NW, C , SE)
                   , (SW, C , NE)
                   )

    def allEq[A](x: List[A]): Boolean = x match {
      case d :: e :: t => d == e && allEq(e::t)
      case _ => true
    }

    // Dammit Scala and your missing libraries.
    // This is not Java where inadequacy is the norm. Stop copying the losers!
    def mapMOption[A, B](f: A => Option[B], as: List[A]): Option[List[B]] =
      as.map(f).foldRight[Option[List[B]]](Some(Nil))((o, z) =>
        for(oo <- o;
            zz <- z)
        yield oo :: zz)

    val isWin = wins exists {
      case (a, b, c) => mapMOption((p: Position) => mm get p, List(a, b, c)) exists (allEq(_))
    }

    val isDraw = Position.positions forall (mm contains _)

    j match {
      case Some(_) => MoveResult.positionAlreadyOccupied
      case None    => if(isWin) MoveResult.gameOver(FinishedBoardB(bb, GameResult.win(whoseTurn)))
                      else if(isDraw) MoveResult.gameOver(FinishedBoardB(bb, Draw))
                      else MoveResult.keepPlaying(bb)
    }
  }

  override def toString = toString((p, _) => p match {
    case None    => ' '
    case Some(p) => p.toSymbol
  }) + "\n" + List("[ ", whoseTurn.toString, " to move ]").mkString
}
private final case class MapBoard(moves: List[(Position, Player)], m: collection.immutable.Map[Position, Player]) extends Board

object Board {
  private def board(moves: List[(Position, Player)], m: M[Position, Player]): Board =
    MapBoard(moves, m)

  def empty = board(Nil, Map.empty)
}

sealed trait FinishedBoard extends BoardLike {
  private def board =
    this match {
      case FinishedBoardB(b, _) => b
    }

  def result =
    this match {
      case FinishedBoardB(_, r) => r
    }

  def takeBack = board.takeBack.fold(
    sys.error("Broken invariant: board in-play with empty move list. This is a program bug")
  , b => b)

  def whoseTurn = board.whoseTurn

  def isEmpty = board.isEmpty

  def nmoves = board.nmoves

  def occupiedPositions = board.occupiedPositions

  def playerAt(p: Position) = board playerAt p

  override def toString = board.toString + "\n" + List("[[ ", result.toString, " ]]").mkString
}

private final case class FinishedBoardB(b: Board, r: GameResult) extends FinishedBoard

sealed trait MoveResult {
  def fold[X](positionAlreadyOccupied: => X,
              keepPlaying: Board => X,
              gameOver: FinishedBoard => X): X =
    this match {
      case PositionAlreadyOccupied => positionAlreadyOccupied
      case KeepPlaying(b) => keepPlaying(b)
      case GameOver(b) => gameOver(b)
    }

  def keepPlaying: Option[Board] = fold(None, Some(_), _ => None)

  def keepPlayingOr[A](els: => A, board: Board => A): A = keepPlaying match {
    case None    => els
    case Some(b) => board(b)
  }

  def -?->(p: Position): MoveResult = keepPlayingOr(this, _ --> p)
}
private case object PositionAlreadyOccupied extends MoveResult
private case class KeepPlaying(b: Board) extends MoveResult
private case class GameOver(b: FinishedBoard) extends MoveResult

object MoveResult {
  def positionAlreadyOccupied: MoveResult = PositionAlreadyOccupied
  def keepPlaying(b: Board): MoveResult = KeepPlaying(b)
  def gameOver(b: FinishedBoard): MoveResult = GameOver(b)
}

object Main {
  def main(args: Array[String]) {
    def surround(e: => Unit) {
      println
      println
      e
      println
    }

    def printBoard[B](inheritance: B => BoardLike, b: B, empty: Position => Char = _ => ' ') =
      surround(println(inheritance(b) toString ((pl, pos) => pl match {
      case Some(z) => z.toSymbol
      case None    => empty(pos)
    })))

    // Scala's readChar is trivially broken.
    def readChar: Option[Char] = {
      val line = java.lang.System.console.readLine 
      if(line.isEmpty) None else Some(line(0))
    }

    @annotation.tailrec
    def gameLoop[B](
                    inheritance: B => BoardLike
                  , move: (Position, B) => Unit
                  , b: B) {
          
      val p = inheritance(b).whoseTurn
      List( 
            p + " to move [" + p.toSymbol + "]"
          , "  [1-9] to Move"
          , "  q to Quit"
          , "  v to view board positions"
          ) foreach println
      print("  > ")
      
      readChar match {
        case None => {
          println("Please make a selection.")
          gameLoop(inheritance, move, b)
        }
        case Some(c) =>       
          if("vV" contains c) {
            printBoard(inheritance, b, _.toChar)
            gameLoop(inheritance, move, b)
          } else Position.fromChar(c) match {
            case None    => if("qQ" contains c) println("Bye!")
                            else {
                              println("Invalid selection. Please try again.")
                              gameLoop(inheritance, move, b)
                            }
            case Some(d) => move(d, b)
          }  
      }
    }

    def tictactoeBoard(b: Board) {
      gameLoop[Board]( 
                      v => v
                    , (p, bb) => bb --> p fold (
                                                  {
                                                    println("That position is already taken. Try again.")
                                                    printBoard[Board](v => v, bb)
                                                    println
                                                    tictactoeBoard(bb)
                                                  }
                                                , bbb => {
                                                    surround(printBoard[Board](v => v, bbb))
                                                    tictactoeBoard(bbb)
                                                  }
                                                , bbb => {
                                                    surround(printBoard[FinishedBoard](v => v, bbb))
                                                    println(bbb.result match {
                                                      case Player1Wins => "Player 1 Wins!"
                                                      case Player2Wins => "Player 2 Wins!"
                                                      case Draw        => "Draw"
                                                    })
                                                  }
                                                )
                    , b)
    }

    gameLoop[EmptyBoard](v => v, (p, b) => {
                                             val bb = b --> p
                                             surround(printBoard[Board](v => v, bb))
                                             tictactoeBoard(bb)
                                           }, EmptyBoard.empty)
  }
}
