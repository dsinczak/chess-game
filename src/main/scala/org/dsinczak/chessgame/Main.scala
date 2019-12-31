package org.dsinczak.chessgame

import com.whitehatgaming.UserInputFile
import org.dsinczak.chessgame.engine.{Board, ChessGame, Pos}

import scala.collection.mutable.ListBuffer

object Main extends App {

  import engine.display.boardShow
  import cats.syntax.show._

  val defaultFile = "sample-moves.txt"

  val moves = loadMoves(if (args.length == 0) defaultFile else args(0))
  println("Loaded moves: ")
  println(moves.map("[" +_+ "]"))

  val gamePlay = ChessGame.run(Board.default(), moves)

  println("-------------------------------------------------------------")
  if(gamePlay.nonEmpty) {
    gamePlay.foreach{ state =>
      println("Messages: " + state.messages)
      println("Board: ")
      println(state.board.show)
      println("\n\n")
    }
  } else {
    println("No game result")
  }

  def loadMoves(fileName: String): List[(Pos, Pos)] = {
    val userInput = new UserInputFile(getClass.getClassLoader.getResource(fileName).getPath)
    var move: Array[Int] = userInput.nextMove()
    var moves = ListBuffer[Array[Int]]()
    while (move != null) {
      move = userInput.nextMove()
      if (move != null) moves += move
    }

    moves.map(m=>(Pos(m(0), 7-m(1)), Pos(m(2), 7-m(3)))).toList
  }
}
