package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.parser.parse
import org.dsinczak.chessgame.engine.{Pos, Queen}
import org.scalatest.{MustMatchers, WordSpec}

class QueenSpec extends WordSpec with MustMatchers {

  " Queen" when {
    "is in default position" should {
      def board = parse(
        """|[r][n][b][q][k][b][n][r]
           |[p][ ][p][p][p][p][ ][p]
           |[ ][ ][ ][ ][ ][ ][ ][ ]
           |[ ][ ][ ][ ][B][ ][ ][ ]
           |[ ][ ][ ][Q][ ][ ][p][ ]
           |[ ][ ][p][ ][ ][ ][ ][ ]
           |[P][P][P][P][P][P][P][P]
           |[R][N][B][ ][K][ ][N][R]""".stripMargin)



      "forbid all" in {
        Queen.isValid(board, Pos("d4"), Pos("e5")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("f6")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("f7")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("b3")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("h3")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("h4")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("f5")) mustBe false
        Queen.isValid(board, Pos("d4"), Pos("e4")) mustBe false

      }

      "allow all" in {
        Queen.isValid(board, Pos("d4"), Pos("c5")) mustBe true
        Queen.isValid(board, Pos("d4"), Pos("b6")) mustBe true
        Queen.isValid(board, Pos("d4"), Pos("e3")) mustBe true
        Queen.isValid(board, Pos("d4"), Pos("c3")) mustBe true
      }
    }
  }

}
