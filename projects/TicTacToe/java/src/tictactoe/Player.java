package tictactoe;

import fj.F;

public enum Player {
  Player1, Player2;

  public Player alternate() {
    return this == Player1 ? Player2 : Player1;
  }

  public char toSymbol() {
    return this == Player1 ? 'X' : 'O';
  }

  @Override
  public String toString() {
    return this == Player1 ? "Player 1" : "Player 2";
  }

  public static final F<Player, Character> toSymbol = new F<Player, Character>() {
    public Character f(final Player p) {
      return p.toSymbol();
    }
  };
}
