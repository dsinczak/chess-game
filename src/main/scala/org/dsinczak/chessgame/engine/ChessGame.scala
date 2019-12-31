package org.dsinczak.chessgame.engine

import scala.collection.mutable.ListBuffer

object ChessGame {

  case class State(board: Board, messages: List[String])

  // Player 1 - Upper case - White - Down
  // Player 2 - Lower case - Black - Up

  // Start: Player1

  def run(board: Board, moves: List[(Pos, Pos)]): List[State] = {
    State(board, List())::run(board, moves, White)
  }

  private def run(board: Board, moves: List[(Pos, Pos)], color: Color): List[State] = {
    moves match {
      case (from, to) :: tail =>
        board.squareState(from) match {
          case Occupied(piece) if piece.isValid(board, from, to) =>
            if (piece.color == color) {
              goToNextState(board, color, from, to, tail)
            } else {
              throw new IllegalArgumentException(s"This round belongs to player $color but source piece is ${piece.color}")
            }
          case Occupied(piece) =>
            val messages = List(s"The move from $from to $to of $piece was invalid. Player $color stays in game.")
            State(board, messages) :: run(board, tail, color)
          case Empty => throw new IllegalStateException(s"Move from $from to $to is a move from empty source.")
        }

      case Nil => List()
    }
  }

  private def goToNextState(board: Board, color: Color, from: Pos, to: Pos, moves: List[(Pos, Pos)]) = {
    val newBoard = board.move(from, to)
    if (newBoard.isCheck(color)) throw new IllegalStateException(s"Player $color finished his round in check. This is prohibited.")

    val messages = ListBuffer[String]()
    if (newBoard.isCheckMate(color.next())) {
      messages + s"Player ${color.next()} is in checkmate"
    } else if (newBoard.isCheck(color.next())) {
      messages + s"Player ${color.next()} is in check"
    }

    State(newBoard, messages.toList) :: run(newBoard, moves, color.next())
  }
}
