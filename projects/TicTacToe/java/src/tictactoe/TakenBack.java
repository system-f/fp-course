package tictactoe;

import fj.F;
import fj.P1;

public abstract class TakenBack {
  private TakenBack() {}

  public abstract <X> X fold(P1<X> isEmpty, F<Board, X> isBoard);

  public static TakenBack isEmpty() {
    return new TakenBack() {
      public <X> X fold(final P1<X> isEmpty, final F<Board, X> isBoard) {
        return isEmpty._1();
      }
    };
  }

  public static TakenBack isBoard(final Board b) {
    return new TakenBack() {
      public <X> X fold(final P1<X> isEmpty, final F<Board, X> isBoard) {
        return isBoard.f(b);
      }
    };
  }
}
