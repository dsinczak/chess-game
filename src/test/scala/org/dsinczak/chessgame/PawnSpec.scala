package org.dsinczak.chessgame

import org.dsinczak.chessgame.engine.parser.parse
import org.dsinczak.chessgame.engine.{Board, Pawn, Pos}
import org.scalatest.{MustMatchers, WordSpec}

class PawnSpec extends WordSpec with MustMatchers {

  " Pawn" when {
    "is in default position" should {
      val board = Board.default()

      "allow all" in {
        Pawn.isValid(board, Pos("c2"), Pos("c3")) mustBe true
        Pawn.isValid(board, Pos("c2"), Pos("c4")) mustBe true
        Pawn.isValid(board, Pos("d7"), Pos("d6")) mustBe true
        Pawn.isValid(board, Pos("d7"), Pos("d5")) mustBe true
      }

      "forbid all" in {
        Pawn.isValid(board, Pos("c2"), Pos("b3")) mustBe false
        Pawn.isValid(board, Pos("c2"), Pos("d3")) mustBe false
        Pawn.isValid(board, Pos("c2"), Pos("c1")) mustBe false
      }
    }

    "is on custom position" should {
      "is in e3 position" should {
        def board = parse(
          """|[r][n][b][q][k][b][n][r]
             |[p][ ][p][p][p][p][p][p]
             |[ ][ ][ ][ ][ ][P][P][ ]
             |[ ][p][ ][ ][ ][ ][ ][ ]
             |[ ][ ][P][ ][ ][ ][ ][ ]
             |[ ][ ][ ][ ][ ][ ][ ][ ]
             |[P][P][ ][P][P][ ][ ][P]
             |[R][N][B][Q][K][B][N][R]""".stripMargin)

        "allow all" in {
          Pawn.isValid(board, Pos("c4"), Pos("c5")) mustBe true
          Pawn.isValid(board, Pos("c4"), Pos("b5")) mustBe true
          Pawn.isValid(board, Pos("f7"), Pos("g6")) mustBe true
        }

        "forbid all" in {
          Pawn.isValid(board, Pos("c4"), Pos("c6")) mustBe false
          Pawn.isValid(board, Pos("c4"), Pos("d5")) mustBe false
          Pawn.isValid(board, Pos("f7"), Pos("f6")) mustBe false
        }
      }
    }
  }
}
