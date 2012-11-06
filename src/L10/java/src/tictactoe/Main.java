package tictactoe;

import fj.*;
import fj.data.Option;

import static fj.Unit.unit;
import static fj.data.Option.some;
import static java.lang.System.out;

public final class Main {
  private Main() {}

  private static void surround(P1<Unit> e) {
    out.println();
    out.println();
    e._1();
    out.println();
  }

  private static <B> void printBoard(final F<B, BoardLike> inheritance, final B b, final F<Position, Character> empty) {
    surround(new P1<Unit>() {
      public Unit _1() {
        out.println(inheritance.f(b).toString(new F2<Option<Player>, Position, Character>() {
          public Character f(final Option<Player> pl, final Position pos) {
            return pl.option(empty.f(pos), Player.toSymbol);
          }
        }));
        return unit();
      }
    });
  }

  private static <B> void printBoardSpaces(final F<B, BoardLike> inheritance, final B b) {
    printBoard(inheritance, b, Function.<Position, Character>constant(' '));
  }

  private static Option<Character> readChar() {
    final String line = System.console().readLine();
    return line.isEmpty() ? Option.<Character>none() : some(line.charAt(0));
  }

  private static <B> void gameLoop(final F<B, BoardLike> inheritance, final F2<Position, B, Unit> move, final B b) {
    final Player p = inheritance.f(b).whoseTurn();
    out.println(p + " to move [" + p.toSymbol() + "]");
    out.println("  [1-9] to Move");
    out.println("  q to Quit");
    out.println("  v to view board positions");
    out.print("  > ");

    readChar().option(new P1<Unit>() {
      public Unit _1() {
        out.println("Please make a selection.");
        gameLoop(inheritance, move, b);
        return unit();
      }
    }, new F<Character, Unit>() {
      public Unit f(final Character c) {
        if(c == 'v' || c == 'V') {
          printBoard(inheritance, b, Position.toChar);
          gameLoop(inheritance, move, b);
          return unit();
        } else {
          return Position.fromChar(c).option(new P1<Unit>() {
            public Unit _1() {
              if(c == 'q' || c == 'Q')
                out.println("Bye!");
              else {
                out.println("Invalid selection. Please try again.");
                gameLoop(inheritance, move, b);
              }
              return unit();
            }
          }, new F<Position, Unit>() {
            public Unit f(final Position d) {
              return move.f(d, b);
            }
          });
        }
      }
    });

  }

  private static final F<Board, BoardLike> boardInheritance = new F<Board, BoardLike>() {
    public BoardLike f(final Board board) {
      return board;
    }
  };

  private static final F<Board.FinishedBoard, BoardLike> finishedBoardInheritance = new F<Board.FinishedBoard, BoardLike>() {
    public BoardLike f(final Board.FinishedBoard board) {
      return board;
    }
  };

  private static final F<Board.EmptyBoard, BoardLike> emptyBoardInheritance = new F<Board.EmptyBoard, BoardLike>() {
    public BoardLike f(final Board.EmptyBoard board) {
      return board;
    }
  };

  private static void tictactoeBoard(final Board b) {
    gameLoop(boardInheritance, new F2<Position, Board, Unit>() {
      public Unit f(final Position p, final Board bb) {
        return bb.moveTo(p).fold(
                                  new P1<Unit>() {
                                    public Unit _1() {
                                      out.println("That position is already taken. Try again.");
                                      printBoardSpaces(boardInheritance, bb);
                                      out.println();
                                      tictactoeBoard(bb);
                                      return unit();
                                    }
                                  }
                                , new F<Board, Unit>() {
                                    public Unit f(final Board bbb) {
                                      surround(new P1<Unit>() {
                                        public Unit _1() {
                                          printBoardSpaces(boardInheritance, bbb);
                                          return unit();
                                        }
                                      });
                                      tictactoeBoard(bbb);
                                      return unit();
                                    }
                                  }
                                , new F<Board.FinishedBoard, Unit>() {
                                    public Unit f(final Board.FinishedBoard bbb) {
                                      surround(new P1<Unit>() {
                                        public Unit _1() {
                                          printBoardSpaces(finishedBoardInheritance, bbb);
                                          out.println(bbb.result().strictFold("Player 1 Wins!", "Player 2 Wins!", "Draw"));
                                          return unit();
                                        }
                                      });
                                      return unit();
                                    }
                                  }
                                );
      }
    }, b);
  }

  public static void main(final String... args) {
    gameLoop(
              emptyBoardInheritance
            , new F2<Position, Board.EmptyBoard, Unit>() {
                public Unit f(final Position p, final Board.EmptyBoard b) {
                  final Board bb = b.moveTo(p);
                  surround(new P1<Unit>() {
                    public Unit _1() {
                      printBoardSpaces(boardInheritance, bb);
                      return unit();
                    }
                  });
                  tictactoeBoard(bb);
                  return unit();
                }
              }
            , Board.EmptyBoard.empty()
            );
  }
}
