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
    assert(SudokuCell.parse('.') == SudokuCell(None))
    assert(SudokuCell.parse('x') == SudokuCell(None))
    assert(SudokuCell.parse('1') == SudokuCell(Some(1)))
    assert(SudokuCell.parse('2') == SudokuCell(Some(2)))
    assert(SudokuCell.parse('3') == SudokuCell(Some(3)))
    assert(SudokuCell.parse('4') == SudokuCell(Some(4)))
    assert(SudokuCell.parse('5') == SudokuCell(Some(5)))
    assert(SudokuCell.parse('6') == SudokuCell(Some(6)))
    assert(SudokuCell.parse('7') == SudokuCell(Some(7)))
    assert(SudokuCell.parse('8') == SudokuCell(Some(8)))
    assert(SudokuCell.parse('9') == SudokuCell(Some(9)))
  }
}

class BoardSpec extends FunSpec {
    val exampleBoard = Board(List(
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),
      List(
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(7)), SudokuCell(None), SudokuCell(None),
        SudokuCell(Some(4)), SudokuCell(Some(8)), SudokuCell(None),
      ),
      List(
        SudokuCell(Some(5)), SudokuCell(Some(7)), SudokuCell(Some(8)),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(4)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),

      List(
        SudokuCell(None), SudokuCell(Some(8)), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(1)),
      ),
      List(
        SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(2)),
        SudokuCell(Some(1)), SudokuCell(Some(7)), SudokuCell(None),
        SudokuCell(Some(9)), SudokuCell(Some(4)), SudokuCell(None),
      ),
      List(
        SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(None),
        SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8)),
      ),

      List(
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(1)),
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(2)), SudokuCell(Some(5)), SudokuCell(None),
      ),
      List(
        SudokuCell(None), SudokuCell(Some(4)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(None), SudokuCell(Some(2)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(6)),
      ),
      List(
        SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(6)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(7)),
      ),
    ))

  it ("Board instance should be initialized") {
    Board(List(
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
      List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(None)
      ),
    ))
  }

  it ("map") {
    val f = (x: Int, y: Int) => exampleBoard(x, y) match {
      case SudokuCell(Some(n)) => SudokuCell(Some(n + 1))
      case SudokuCell(None) => SudokuCell(None)
    }
    assert(exampleBoard.map(f).cells(0) == List(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(10)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
    ))
  }

  it ("rotate") {
    val board = exampleBoard.rotate
    assert(board(0) == List(
      SudokuCell(Some(2)), SudokuCell(None), SudokuCell(Some(8)),
      SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(5)), SudokuCell(Some(3)), SudokuCell(None)
    ))
  }

  it ("countSet") {
    assert(exampleBoard.countSet == Set(
      5, 5, 5, 5, 5, 5, 5, 7, 5,
    ))
  }

  it ("countMap") {
    assert(exampleBoard.countMap == Map(
      1 -> 5,
      2 -> 5,
      3 -> 5,
      4 -> 5,
      5 -> 5,
      6 -> 5,
      7 -> 5,
      8 -> 7,
      9 -> 5,
      ))
  }

  it ("apply") {
    assert(exampleBoard(0) == List(
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
      SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
    ))
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
