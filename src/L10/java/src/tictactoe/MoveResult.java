package tictactoe;

import fj.F;
import fj.Function;
import fj.P;
import fj.P1;
import fj.data.Option;

import static fj.P.p;

public abstract class MoveResult {
  private MoveResult() {}

  public abstract <X> X fold(P1<X> positionAlreadyOccupied, F<Board, X> keepPlaying, F<Board.FinishedBoard, X> gameOver);

  public Option<Board> keepPlaying() {
    return fold(
                 p(Option.<Board>none())
               , Option.<Board>some_()
               , Function.<Board.FinishedBoard, Option<Board>>constant(Option.<Board>none())
               );
  }

  public <A> A keepPlayingOr(final P1<A> els, final F<Board, A> board) {
    return keepPlaying().option(els, board);
  }

  public MoveResult tryMove(final Position p) {
    return keepPlayingOr(P.p(this), new F<Board, MoveResult>() {
      public MoveResult f(final Board board) {
        return board.moveTo(p);
      }
    });
  }

  public static MoveResult positionAlreadyOccupied() {
    return new MoveResult() {
      public <X> X fold(final P1<X> positionAlreadyOccupied, final F<Board, X> keepPlaying, final F<Board.FinishedBoard, X> gameOver) {
        return positionAlreadyOccupied._1();
      }
    };
  }

  public static MoveResult keepPlaying(final Board b) {
    return new MoveResult() {
      public <X> X fold(final P1<X> positionAlreadyOccupied, final F<Board, X> keepPlaying, final F<Board.FinishedBoard, X> gameOver) {
        return keepPlaying.f(b);
      }
    };
  }

  public static MoveResult gameOver(final Board.FinishedBoard b) {
    return new MoveResult() {
      public <X> X fold(final P1<X> positionAlreadyOccupied, final F<Board, X> keepPlaying, final F<Board.FinishedBoard, X> gameOver) {
        return gameOver.f(b);
      }
    };
  }
}
