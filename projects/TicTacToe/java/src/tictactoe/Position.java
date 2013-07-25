package tictactoe;

import fj.F;
import fj.data.List;
import fj.data.Option;

import static fj.data.List.list;
import static fj.data.Option.none;
import static fj.data.Option.some;

public enum Position {
  NW, N, NE, W, C, E, SW, S, SE;

  public int toInt() {
    return ordinal() + 1;
  }

  public char toChar() {
    return (char)(toInt() + '0');
  }

  public static List<Position> positions() {
    return list(NW, N, NE, W, C, E, SW, S, SE);
  }

  public static Option<Position> fromInt(final int n) {
    switch(n) {
      case 1: return some(NW);
      case 2: return some(N );
      case 3: return some(NE);
      case 4: return some(W );
      case 5: return some(C );
      case 6: return some(E );
      case 7: return some(SW);
      case 8: return some(S);
      case 9: return some(SE);
      default: return none();
    }
  }

  public static Option<Position> fromChar(final char c) {
    return fromInt(c - 48);
  }

  public final static F<Position, Character> toChar = new F<Position, Character>() {
    public Character f(final Position p) {
      return p.toChar();
    }
  };
}
