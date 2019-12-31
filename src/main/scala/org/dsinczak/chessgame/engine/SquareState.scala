package org.dsinczak.chessgame.engine

sealed trait SquareState {
  def isOccupied: Boolean = this match {
    case Occupied(_) => true
    case _ => false
  }

  def isNotOccupied: Boolean = !isOccupied
}

case object Empty extends SquareState

final case class Occupied(piece: Piece) extends SquareState