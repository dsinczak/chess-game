package org.dsinczak.chessgame

package object engine {

  object display {

    import cats.Show

    implicit val pieceShow: Show[Piece] = (piece: Piece) => {
      val mnemonic = piece.pieceType match {
        case King => "k"
        case Pawn => "p"
        case Rook => "r"
        case Queen => "q"
        case Bishop => "b"
        case Knight => "n"
      }

      piece.color match {
        case White => mnemonic.toUpperCase()
        case Black => mnemonic
      }
    }

    // cross-cutting concern, not business related, therefore
    // quality is poor as time is a factor
    implicit val boardShow: Show[Board] = (board:Board) => {
      val res = StringBuilder.newBuilder
      res.append("_|__a__b__c__d__e__f__g__h_\n")
      var currRowNum = 0
      val rowSize = board.state.length
      for (row <- board.state) {
        currRowNum+=1
        var currColNum = 0
        for (col <- row) {
          if(currColNum==0) {
            res.append(s"${rowSize+1-currRowNum}| ")
          }
          currColNum+=1
          col match {
            case Empty => res.append("[ ]")
            case Occupied(piece) => res.append(s"[${pieceShow.show(piece)}]")
          }
        }
        res.append("\n")
      }
      res.toString().trim()
    }

    // cross-cutting concern, not business related, therefore
    // quality is poor as time is a factor
    implicit val coordinatesBoardShow: Show[Board] = (board:Board) => {
      val res = StringBuilder.newBuilder
      res.append("_|__0__1__2__3__4__5__6__7_\n")
      var currRowNum = 0
      for (row <- board.state) {
        currRowNum+=1
        var currColNum = 0
        for (col <- row) {
          if(currColNum==0) {
            res.append(s"${currRowNum-1}| ")
          }
          currColNum+=1
          col match {
            case Empty => res.append("[ ]")
            case Occupied(piece) => res.append(s"[${pieceShow.show(piece)}]")
          }
        }
        res.append("\n")
      }
      res.toString().trim()
    }

    implicit val testBoardShow: Show[Board] = (board:Board) => {
      val res = StringBuilder.newBuilder
      for (row <- board.state) {
        for (col <- row) {
          col match {
            case Empty => res.append("[ ]")
            case Occupied(piece) => res.append(s"[${pieceShow.show(piece)}]")
          }
        }
        res.append("\n")
      }
      res.toString().trim()
    }
  }

  // Only for test and internal use purposes
  // Unsafe: can fail in case of wrongly formatted string
  private[chessgame] object parser {
    private val rowPattern = "\\[(.*?)\\]".r

    def parse(string:String): Board =
      new Board(string.split("\n").map(parseRow).toVector)

    private def parseRow(row: String) : Vector[SquareState] =
      (for (figure <- rowPattern.findAllMatchIn(row)) yield {
        figure.toString() match {
          case "[ ]" => Empty
          case "[Q]" => Occupied(Piece(Queen, White))
          case "[K]" => Occupied(Piece(King, White))
          case "[R]" => Occupied(Piece(Rook, White))
          case "[N]" => Occupied(Piece(Knight, White))
          case "[B]" => Occupied(Piece(Bishop, White))
          case "[P]" => Occupied(Piece(Pawn, White))
          case "[q]" => Occupied(Piece(Queen, Black))
          case "[k]" => Occupied(Piece(King, Black))
          case "[r]" => Occupied(Piece(Rook, Black))
          case "[n]" => Occupied(Piece(Knight, Black))
          case "[b]" => Occupied(Piece(Bishop, Black))
          case "[p]" => Occupied(Piece(Pawn, Black))
        }
      }).toVector
  }

}
