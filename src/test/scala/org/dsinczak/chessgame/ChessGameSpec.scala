package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.parser.parse
import org.dsinczak.chessgame.engine.{Board, ChessGame, Pos}
import org.scalatest.{MustMatchers, WordSpec}

class ChessGameSpec extends WordSpec with MustMatchers {

  "When playing chess game" should {
    val board = Board.default()

    "white pawn take black one in 2 moves" in {
      import cats.syntax.show._
      import engine.display.testBoardShow

      ChessGame.run(board,
        List(
          (Pos("c2"), Pos("c4")),
          (Pos("d7"), Pos("d5")),
          (Pos("c4"), Pos("d5"))
        )
      ).last.board.show mustBe parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][ ][p][p][p][p]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][P][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[P][P][ ][P][P][P][P][P]
           |[R][N][B][Q][K][B][N][R]""".stripMargin
      ).show
    }

    "apply sample moves" in {

      import cats.syntax.show._
      import engine.display.testBoardShow

      ChessGame.run(board,
        List(
          (Pos(4, 6), Pos(4, 4)),
          (Pos(1, 0), Pos(2, 2)),
          (Pos(3, 6), Pos(3, 5)),
          (Pos(7, 1), Pos(7, 2)),
          (Pos(2, 7), Pos(4, 5)),
          (Pos(7, 0), Pos(7, 1))
        )
      ).last.board.show mustBe parse(
        """|[r][ ][b][q][k][b][n][ ]
           |[p][p][p][p][p][p][p][r]
           |[ ][ ][n][ ][ ][ ][ ][p]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][P][ ][ ][ ]
           |[ ][ ][ ][P][B][ ][ ][ ]
           |[P][P][P][ ][ ][P][P][P]
           |[R][N][ ][Q][K][B][N][R]""".stripMargin
      ).show

    }
  }

}
