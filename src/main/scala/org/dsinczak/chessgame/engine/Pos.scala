package org.dsinczak.chessgame.engine

// TODO this position implementation is clearly 8x8 biased
// i could make it more generic... but time is a factor
case class Pos(col: Int, row: Int) {
  private[engine] def coordinates(): (Int, Int) = (row, col)

  def +(pos: Pos): Pos = {
    Pos(col+pos.col, row+pos.row)
  }

  def *(scalar: Int): Pos = {
    Pos(col*scalar, row*scalar)
  }
}

object Pos {

  private val columnMapping = Map(
    'a' -> 0, 'b' -> 1, 'c' -> 2,
    'd' -> 3, 'e' -> 4, 'f' -> 5,
    'g' -> 6, 'h' -> 7
  )

  def apply(rawValue: String): Pos = {
    new Pos(Pos.columnMapping(rawValue.charAt(0)), 8 - rawValue.charAt(1).toString.toInt)
  }
}