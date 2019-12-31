package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.{Pos, Rook}
import org.scalatest.{MustMatchers, WordSpec}

class RookSpec extends WordSpec with MustMatchers {

  import engine.parser._

  " Rook" when {

    "is in f4 position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][p][p][p][p][p]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][N][ ][R][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[R][ ][B][Q][K][B][N][ ]""".stripMargin)

      "forbid all" in {
        Rook.isValid(board, Pos("f4"), Pos("f8")) mustBe false
        Rook.isValid(board, Pos("f4"), Pos("c4")) mustBe false
        Rook.isValid(board, Pos("f4"), Pos("a4")) mustBe false
        Rook.isValid(board, Pos("f4"), Pos("b4")) mustBe false
        Rook.isValid(board, Pos("f4"), Pos("f2")) mustBe false
        Rook.isValid(board, Pos("f4"), Pos("f1")) mustBe false
      }

      "allow all" in {
        Rook.isValid(board, Pos("f4"), Pos("f7")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("f6")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("f5")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("f3")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("e4")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("g4")) mustBe true
        Rook.isValid(board, Pos("f4"), Pos("h4")) mustBe true
      }
    }
  }
}
