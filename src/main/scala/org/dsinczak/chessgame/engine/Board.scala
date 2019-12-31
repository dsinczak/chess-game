package org.dsinczak.chessgame.engine

import org.dsinczak.chessgame.engine.Board.{BoardSize, BoardState}

/**
 * The whole idea is to represent game as stream of board modifications.
 * Where next state is a new board with applied move. This way "by accident"
 * we have whole game history. The same can be achieved by keeping initial state
 * stream of change events.
 *
 * @param state board state
 */
case class Board private(state: BoardState) {

  private[engine] def this(sq: Vector[Vector[SquareState]]) {
    this(sq.map(_.toArray).toArray)
  }

  def squareState(pos: Pos): SquareState = {
    val (row, col) = pos coordinates()
    state(row)(col)
  }

  def move(from: Pos, to: Pos): Board = {
    val (fromRow, fromCol) = from.coordinates()
    val (toRow, toCol) = to.coordinates()

    val newState = Board.copyOf(state)
    val fromState = state(fromRow)(fromCol)
    newState(fromRow)(fromCol) = Empty
    newState(toRow)(toCol) = fromState
    Board(newState)
  }

  private[engine] def isEmpty(pos: Pos): Boolean = squareState(pos) == Empty

  private[engine] def size(): BoardSize = BoardSize(state.length, state(0).length)

  // TODO sorry but i did not managed to finish this just no time, but the idea
  // of how to do it is simple
  private[engine] def isCheck(color: Color): Boolean = false

  // TODO sorry but i did not managed to finish this just no time, but the idea
  // of how to do it is simple
  private[engine] def isCheckMate(color: Color): Boolean = false

}

object Board {

  private[engine] final case class BoardSize(rows:Int, cols:Int)
  private type BoardState = Array[Array[SquareState]]

  def empty(): Board = {
    // theoretically... it can be any size
    new Board(Vector.tabulate(8, 8)((_, _) => Empty))
  }

  import parser._

  def default(): Board = {
    parse(
      """|[r][n][b][q][k][b][n][r]
         |[p][p][p][p][p][p][p][p]
         |[ ][ ][ ][ ][ ][ ][ ][ ]
         |[ ][ ][ ][ ][ ][ ][ ][ ]
         |[ ][ ][ ][ ][ ][ ][ ][ ]
         |[ ][ ][ ][ ][ ][ ][ ][ ]
         |[P][P][P][P][P][P][P][P]
         |[R][N][B][Q][K][B][N][R]""".stripMargin)
  }

  // Deep 2dim array copy
  private def copyOf(as: BoardState): BoardState = {
    val cas = Array.ofDim[Array[SquareState]](as.length)
    for (i <- as.indices) {
      val a = as(i)
      cas(i) = java.util.Arrays.copyOf(a, a.length)
    }
    cas
  }
}
