package sudoku

import org.scalatest.FunSpec

class SudokuCellSpec extends FunSpec {
  it ("SudokuCell should be initialized") {
    SudokuCell(Some(3))
  }

  describe ("toString") {
    it ("string of defined cell is the number") {
      assert(SudokuCell(Some(3)).toString == "3")
    }
    it ("string of undefined cell is dot") {
      assert(SudokuCell(None).toString == ".")
    }
  }

  it ("parse") {
    assert(SudokuCell.parse(".") == SudokuCell(None))
    assert(SudokuCell.parse("x") == SudokuCell(None))
    assert(SudokuCell.parse("1") == SudokuCell(Some(1)))
    assert(SudokuCell.parse("2") == SudokuCell(Some(2)))
    assert(SudokuCell.parse("3") == SudokuCell(Some(3)))
    assert(SudokuCell.parse("4") == SudokuCell(Some(4)))
    assert(SudokuCell.parse("5") == SudokuCell(Some(5)))
    assert(SudokuCell.parse("6") == SudokuCell(Some(6)))
    assert(SudokuCell.parse("7") == SudokuCell(Some(7)))
    assert(SudokuCell.parse("8") == SudokuCell(Some(8)))
    assert(SudokuCell.parse("9") == SudokuCell(Some(9)))
  }

  it ("numbers") {
    assert(Board.numbers(7) == Set(1, 2, 3, 4, 5, 6, 7))
    assert(Board.numbers(9) == Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  it ("nuberStrings") {
    assert(Board.numberStrings(9) == Set("1", "2", "3", "4", "5", "6", "7", "8", "9"))
  }
}

class BoardSpec extends FunSpec {
    val exampleBoard = Board(Seq(
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(7)), SudokuCell(None), SudokuCell(None),
        SudokuCell(Some(4)), SudokuCell(Some(8)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(5)), SudokuCell(Some(7)), SudokuCell(Some(8)),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(4)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),

      Seq(
        SudokuCell(None), SudokuCell(Some(8)), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(1)),
      ),
      Seq(
        SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(2)),
        SudokuCell(Some(1)), SudokuCell(Some(7)), SudokuCell(None),
        SudokuCell(Some(9)), SudokuCell(Some(4)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(None),
        SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8)),
      ),

      Seq(
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(1)),
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(2)), SudokuCell(Some(5)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(None), SudokuCell(Some(4)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(None), SudokuCell(Some(2)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(6)),
      ),
      Seq(
        SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(6)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(7)),
      ),
    ))

  it ("Board instance should be initialized") {
    Board(Seq(
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
    ))
  }

  it ("changeBoard") {
    val resBoard = Board.parse(
      ".3...9..." +
      "36.7..48." +
      "578..4..." +

      ".8...36.1" +
      ".5217.94." +
      "4.392..78" +

      "89136.25." +
      ".458.2.16" +
      "236.15897").get
    assert(exampleBoard.changeBoard(1, 0, SudokuCell(Some(3))) == resBoard)
  }

  it ("map") {
    val f = (x: Int, y: Int) => exampleBoard(x, y) match {
      case SudokuCell(Some(n)) => SudokuCell(Some(n + 1))
      case SudokuCell(None) => SudokuCell(None)
    }
    assert(exampleBoard.map(f).cells(0) == Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(10)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
    ))
  }

  it ("rotate") {
    val board = exampleBoard.rotate
    assert(board(0) == Seq(
      SudokuCell(Some(2)), SudokuCell(None), SudokuCell(Some(8)),
      SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(5)), SudokuCell(Some(3)), SudokuCell(None)
    ))

    assert(exampleBoard.rotate.rotate.rotate.rotate == exampleBoard)
  }

  it ("flip") {
    val board = exampleBoard.flip
    assert(board(2) == Seq(
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(8)), SudokuCell(Some(7)), SudokuCell(Some(5))
    ))
  }

  it ("size") {
    val board = Board(Seq(
      Seq(SudokuCell(Some(1)), SudokuCell(Some(2)), SudokuCell(Some(3))),
      Seq(SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(4)))
    ))
    assert (board.size == (3, 2))
  }

  it ("apply") {
    assert(exampleBoard(0) == Seq(
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
      SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
    ))
  }

  it ("toPrettyString") {
    assert(exampleBoard.toPrettyString ==
      ".....9...\n" +
      "36.7..48.\n" +
      "578..4...\n" +

      ".8...36.1\n" +
      ".5217.94.\n" +
      "4.392..78\n" +

      "89136.25.\n" +
      ".458.2.16\n" +
      "236.15897"
    )
  }

  it ("toString") {
    assert(exampleBoard.toString ==
      ".....9..." +
      "36.7..48." +
      "578..4..." +

      ".8...36.1" +
      ".5217.94." +
      "4.392..78" +

      "89136.25." +
      ".458.2.16" +
      "236.15897"
    )
  }

  describe ("parse") {
    it ("example sudoku") {
      val boardString = exampleBoard.toString
      assert(Board.parse(boardString) == Some(exampleBoard))
    }
    it ("large string") {
      val boardString = exampleBoard.toString + "123"
      assert(Board.parse(boardString) == None)
    }
    it ("strange charactor") {
      val boardString = exampleBoard.toString.replace('.', 'x')
      assert(Board.parse(boardString) == None)
    }
  }
}

