package org.dsinczak.chessgame.engine

import org.dsinczak.chessgame.engine.Board.BoardSize

import scala.collection.{mutable, _}
import scala.util.{Success, Try}

case class Piece(pieceType: Type, color: Color) {
  def isValid(board: Board, from: Pos, to: Pos): Boolean = pieceType.isValid(board, from, to)
}

sealed trait Color {
  def next(): Color;
}
case object White extends Color {
  override def next(): Color = Black
}
case object Black extends Color {
  override def next(): Color = White
}

sealed trait Type {

  protected def validateDestination(board: Board, from: Pos, to: Pos): Boolean =
    (board.squareState(from), board.squareState(to)) match {
      case (_, Empty) => true
      case (Occupied(fromPiece), Occupied(toPiece)) => fromPiece.color != toPiece.color
      case _ => false
    }

  def isValid(board: Board, from: Pos, to: Pos): Boolean
}

case object Pawn extends Type {

  private def isFirstMove(boardSize: BoardSize, pos: Pos, color: Color): Boolean = {
    color match {
      case White => pos.row == boardSize.rows - 2
      case Black => pos.row == 1
    }
  }

  private def forwardStep(color: Color): Pos = color match {
    case White => Pos(0, -1)
    case Black => Pos(0, 1)
  }

  private def diagonalSteps(color: Color): (Pos, Pos) = color match {
    case White => (Pos(1, -1), Pos(-1, -1))
    case Black => (Pos(1, 1), Pos(-1, -1))
  }

  private def isOccupiedByEnemy(board: Board, pos: Pos, color: Color): Boolean = Try(board.squareState(pos)) match {
    case Success(Occupied(Piece(_, occupiedColor))) if occupiedColor != color => true
    case _ => false
  }

  def isValid(board: Board, from: Pos, to: Pos): Boolean = {
    val color = board.squareState(from).asInstanceOf[Occupied].piece.color
    val fwdStep = forwardStep(color)
    val (diag1Step, diag2Step) = diagonalSteps(color)

    // Possible steps
    val oneStepForward = from+fwdStep
    val twoStepForward = from+(fwdStep*2)
    val diag1StepForward = from+diag1Step
    val diag2StepForward = from+diag2Step

    val allowed: mutable.Set[Pos] = mutable.Set()
    // Step forward (if empty)
    if(board.isEmpty(oneStepForward)) allowed.add(oneStepForward)
    // Two step forward if empty and its pawn first move
    if(isFirstMove(board.size(), from, color) && board.isEmpty(twoStepForward)) allowed.add(twoStepForward)
    // Diagonal left when enemy is there
    if(isOccupiedByEnemy(board, diag1StepForward, color)) allowed.add(diag1StepForward)
    // Diagonal right when enemy is there
    if(isOccupiedByEnemy(board, diag2StepForward, color)) allowed.add(diag2StepForward)


    allowed.contains(to)
  }

}

/**
 * TODO this can be optimized and generalized, to there was not time
 */
case object Rook extends Type {
  def isValid(board: Board, from: Pos, to: Pos): Boolean = {
    validateDestination(board, from, to) && isWayFree(board, from, to)
  }

  // Very 'brut-force' way of checking if there is nothing between
  // from and to destinations (exclusively)
  private def isWayFree(board: Board, from: Pos, to: Pos): Boolean = {
    val (fromRow, fromCol) = from.coordinates()
    val (toRow, toCol) = to.coordinates()
    if (fromRow == toRow && fromCol != toCol) { // W - E
      if (fromCol - toCol > 0) { // W
        return ((toCol + 1) until fromCol)
          .map(c => Pos(c, fromRow))
          .filter(p => p != from)
          .forall(p => board.isEmpty(p))
      } else { // E
        return ((fromCol + 1) until toCol)
          .map(c => Pos(c, fromRow))
          .filter(p => p != from)
          .forall(p => board.isEmpty(p))
      }
    } else if (fromCol == toCol && fromRow != toRow) { // N - S
      if (fromRow - toRow > 0) { // N
        return ((toRow + 1) until fromRow)
          .map(r => Pos(fromCol, r))
          .filter(p => p != from)
          .forall(p => board.isEmpty(p))
      } else { // S
        return ((fromRow + 1) until toRow)
          .map(r => Pos(fromCol, r))
          .filter(p => p != from)
          .forall(p => board.isEmpty(p))
      }
    }
    true
  }
}

case object Bishop extends Type {
  def isValid(board: Board, from: Pos, to: Pos): Boolean =
    isOnDiagonal(from: Pos, to: Pos) && validateDestination(board, from, to) && isWayFree(board, from, to)

  // Very 'brut-force' way of checking if there is nothing between
  // from and to destinations (exclusively)
  private def isWayFree(board: Board, from: Pos, to: Pos): Boolean = {
    val (fromRow, fromCol) = from.coordinates()
    val (toRow, toCol) = to.coordinates()
    if (fromCol - toCol > 0) { // NW/SW
      if (fromRow - toRow > 0) { // NW
        return checkQuarter(board, from, to, Pos(-1, -1))
      } else { // SW
        return checkQuarter(board, from, to, Pos(-1, 1))
      }
    } else if (fromCol - toCol < 0) { // NE/SE
      if (fromRow - toRow > 0) { // NE
        return checkQuarter(board, from, to, Pos(1, -1))
      } else { // SE
        return checkQuarter(board, from, to, Pos(1, 1))
      }
    }
    true
  }

  private def checkQuarter(board: Board, from: Pos, to: Pos, step: Pos): Boolean = {
    var checkPos = from
    while (checkPos != to) {
      checkPos = checkPos + step
      if (checkPos != to && !board.isEmpty(checkPos)) {
        return false
      }
    }
    true
  }

  private def isOnDiagonal(from: Pos, to: Pos): Boolean =
    Math.abs(from.row - to.row) == Math.abs(from.col - to.col)
}

/**
 * Queen is a sum of Rook and Bishop
 * TODO we can optimize this because validateDestination is called 2x
 */
case object Queen extends Type {
  def isValid(board: Board, from: Pos, to: Pos): Boolean = {

    Bishop.isValid(board, from, to) && Rook.isValid(board, from, to)
  }
}

// OK
case object Knight extends Type {
  def isValid(board: Board, from: Pos, to: Pos): Boolean = {
    val (row, col) = from.coordinates()
    val allowed = Set(
      (row - 2, col - 1),
      (row - 1, col - 2),
      (row - 2, col + 1),
      (row - 1, col + 2),
      (row + 2, col - 1),
      (row + 1, col - 2),
      (row + 2, col + 1),
      (row + 1, col + 2))

    allowed.contains(to.coordinates()) && validateDestination(board, from, to)
  }
}

// OK
case object King extends Type {

  def isValid(board: Board, from: Pos, to: Pos): Boolean = {
    val (row, col) = from.coordinates()
    val allowed = Set(
      (row - 1, col),
      (row - 1, col - 1),
      (row - 1, col + 1),
      (row, col - 1),
      (row, col + 1),
      (row + 1, col),
      (row + 1, col - 1),
      (row + 1, col + 1))

    allowed.contains(to.coordinates()) && validateDestination(board, from, to)
  }
}