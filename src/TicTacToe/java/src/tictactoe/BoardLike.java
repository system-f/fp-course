package tictactoe;

import fj.F;
import fj.F2;
import fj.P1;
import fj.data.List;
import fj.data.Option;

import static fj.Monoid.stringMonoid;
import static fj.data.List.list;
import static tictactoe.Position.*;

public abstract class BoardLike {
  public abstract Player whoseTurn();
  public final Player whoseNotTurn() {
    return whoseTurn().alternate();
  }
  public abstract boolean isEmpty();
  public abstract List<Position> occupiedPositions();
  public abstract int nmoves();
  public abstract Option<Player> playerAt(Position p);
  public final Player playerAtOr(final Position p, final P1<Player> or) {
    return playerAt(p).orSome(or);
  }
  public final boolean isOccupied(final Position p) {
    return playerAt(p).isSome();
  }
  public final boolean isNotOccupied(final Position p) {
    return !isOccupied(p);
  }
  public final String toString(final F2<Option<Player>, Position, Character> f) {
    final String z = ".===.===.===.";
    final F<Position, String> k = new F<Position, String>() {
      public String f(final Position p) {
        return f.f(playerAt(p), p).toString();
      }
    };


    final List<String> i =
        list(
              z
            , stringMonoid.sumLeft().f(list("| ", k.f(NW), " | ", k.f(N ), " | ", k.f(NE), " |"))
            , z
            , stringMonoid.sumLeft().f(list("| ", k.f( W), " | ", k.f(C ), " | ", k.f( E), " |"))
            , z
            , stringMonoid.sumLeft().f(list("| ", k.f(SW), " | ", k.f(S ), " | ", k.f(SE), " |"))
            , z
            ).intersperse("\n");

    return stringMonoid.sumLeft().f(i);
  }
}
