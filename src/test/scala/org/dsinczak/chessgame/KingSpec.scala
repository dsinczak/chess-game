package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.{Board, King, Pos}
import org.scalatest.{MustMatchers, WordSpec}

class KingSpec extends WordSpec with MustMatchers {

  import engine.parser._

  "King" when {
    "is in default position" should {
      val board = Board.default()

      "forbid all" in {
        King.isValid(board, Pos("e1"), Pos("d1")) mustBe false
        King.isValid(board, Pos("e1"), Pos("f1")) mustBe false
        King.isValid(board, Pos("e1"), Pos("e2")) mustBe false
        King.isValid(board, Pos("e1"), Pos("f2")) mustBe false
        King.isValid(board, Pos("e1"), Pos("d2")) mustBe false
      }
    }

    "is in e3 position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][p][p][p][p][p]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][K][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[R][N][B][Q][ ][B][N][R]""".stripMargin)

      "forbid all" in {
        King.isValid(board, Pos("e3"), Pos("d2")) mustBe false
        King.isValid(board, Pos("e3"), Pos("e2")) mustBe false
        King.isValid(board, Pos("e3"), Pos("f2")) mustBe false
        King.isValid(board, Pos("e3"), Pos("g4")) mustBe false
      }

      "allow all" in {
        King.isValid(board, Pos("e3"), Pos("d3")) mustBe true
        King.isValid(board, Pos("e3"), Pos("f3")) mustBe true
        King.isValid(board, Pos("e3"), Pos("d4")) mustBe true
        King.isValid(board, Pos("e3"), Pos("e4")) mustBe true
        King.isValid(board, Pos("e3"), Pos("f4")) mustBe true
      }
    }

    "is in c6 position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][p][p][p][p][p]
           |[ ][ ][K][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[R][N][B][Q][ ][B][N][R]""".stripMargin)

      "allow all" in {
        King.isValid(board, Pos("c6"), Pos("b7")) mustBe true
        King.isValid(board, Pos("c6"), Pos("c7")) mustBe true
        King.isValid(board, Pos("c6"), Pos("d7")) mustBe true

        King.isValid(board, Pos("c6"), Pos("b6")) mustBe true
        King.isValid(board, Pos("c6"), Pos("d6")) mustBe true

        King.isValid(board, Pos("c6"), Pos("b5")) mustBe true
        King.isValid(board, Pos("c6"), Pos("c5")) mustBe true
        King.isValid(board, Pos("c6"), Pos("d5")) mustBe true
      }
    }

  }

}
