package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.{Board, Knight, Pos}
import org.scalatest.{MustMatchers, WordSpec}

class KnightSpec extends WordSpec with MustMatchers {

  import engine.parser._

  "King" when {
    "is in default position" should {
      val board = Board.default()

      "forbid all" in {
        Knight.isValid(board, Pos("b1"), Pos("a1")) mustBe false
        Knight.isValid(board, Pos("b1"), Pos("a2")) mustBe false
        Knight.isValid(board, Pos("b1"), Pos("b2")) mustBe false
        Knight.isValid(board, Pos("b1"), Pos("c2")) mustBe false
        Knight.isValid(board, Pos("b1"), Pos("c1")) mustBe false
        Knight.isValid(board, Pos("b1"), Pos("b3")) mustBe false
      }

      "allow all" in {
        Knight.isValid(board, Pos("b1"), Pos("a3")) mustBe true
        Knight.isValid(board, Pos("b1"), Pos("c3")) mustBe true
      }
    }

    "is in d6 position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][p][p][p][p][p]
           |[ ][ ][ ][N][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[R][ ][B][Q][K][B][N][R]""".stripMargin)

      "forbid all" in {
        Knight.isValid(board, Pos("d6"), Pos("d7")) mustBe false
        Knight.isValid(board, Pos("d6"), Pos("e6")) mustBe false
        Knight.isValid(board, Pos("d6"), Pos("d5")) mustBe false
        Knight.isValid(board, Pos("d6"), Pos("c5")) mustBe false
      }
      "allow all" in {
        Knight.isValid(board, Pos("d6"), Pos("e8")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("c8")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("e4")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("c4")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("f7")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("b7")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("f5")) mustBe true
        Knight.isValid(board, Pos("d6"), Pos("b5")) mustBe true
      }
    }
  }

}
