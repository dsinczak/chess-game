package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine._
import org.scalatest.{MustMatchers, WordSpec}

class BoardSpec extends WordSpec with MustMatchers {

  "Board" when {
    val board = Board.default()

    "squareState is requested" should {
      "return pawn from a7" in {
        board.squareState(Pos("a7")) mustBe Occupied(Piece(Pawn, Black))
      }

      "return pawn from d8" in {
        board.squareState(Pos("d8")) mustBe Occupied(Piece(Queen, Black))
      }

      "return Pawn from d2" in {
        board.squareState(Pos("d2")) mustBe Occupied(Piece(Pawn, White))
      }

      "return Bishop from 2" in {
        board.squareState(Pos("c1")) mustBe Occupied(Piece(Bishop, White))
      }
    }

    "piece is moved" should {
      import cats.syntax.show._
      import engine.display.testBoardShow

      "place Pawn from e2 to " in {
        board.move(Pos("e2"), Pos("e4")).show mustBe
          """|[r][n][b][q][k][b][n][r]
             |[p][p][p][p][p][p][p][p]
             |[ ][ ][ ][ ][ ][ ][ ][ ]
             |[ ][ ][ ][ ][ ][ ][ ][ ]
             |[ ][ ][ ][ ][P][ ][ ][ ]
             |[ ][ ][ ][ ][ ][ ][ ][ ]
             |[P][P][P][P][ ][P][P][P]
             |[R][N][B][Q][K][B][N][R]""".stripMargin
      }

      "make series of movements " in {
        board
          .move(Pos("e2"), Pos("e4"))
          .move(Pos("b1"), Pos("c3"))
          .move(Pos("b8"), Pos("a6"))
          .move(Pos("d1"), Pos("h5"))
          .show mustBe
          """|[r][ ][b][q][k][b][n][r]
             |[p][p][p][p][p][p][p][p]
             |[n][ ][ ][ ][ ][ ][ ][ ]
             |[ ][ ][ ][ ][ ][ ][ ][Q]
             |[ ][ ][ ][ ][P][ ][ ][ ]
             |[ ][ ][N][ ][ ][ ][ ][ ]
             |[P][P][P][P][ ][P][P][P]
             |[R][ ][B][ ][K][B][N][R]""".stripMargin
      }
    }
  }
}
