package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.{Bishop, Pos}
import org.scalatest.{MustMatchers, WordSpec}

class BishopSpec extends WordSpec with MustMatchers {

  import engine.parser._

  " Bishop" when {

    "is in e6 position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][p][p][p][p][p][p][p]
           |[ ][ ][R][ ][B][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[ ][N][B][Q][K][ ][N][R]""".stripMargin)

      "forbid all" in {
        Bishop.isValid(board, Pos("e6"), Pos("c8")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("g8")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("c6")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("b6")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("e8")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("e7")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("e2")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("e5")) mustBe false
        Bishop.isValid(board, Pos("e6"), Pos("e4")) mustBe false
      }

      "allow all" in {
        Bishop.isValid(board, Pos("e6"), Pos("d7")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("f7")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("f5")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("g4")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("h3")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("d5")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("c4")) mustBe true
        Bishop.isValid(board, Pos("e6"), Pos("b3")) mustBe true
      }
    }
  }

}
