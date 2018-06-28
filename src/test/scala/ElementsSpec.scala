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

    assert(exampleBoard.rotate.rotate.rotate.rotate == exampleBoard)
  }

  it ("flip") {
    val board = exampleBoard.flip
    assert(board(2) == List(
      SudokuCell(None), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
      SudokuCell(Some(8)), SudokuCell(Some(7)), SudokuCell(Some(5))
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

  it ("size") {
    val board = Board(List(
      List(SudokuCell(Some(1)), SudokuCell(Some(2)), SudokuCell(Some(3))),
      List(SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(4)))
    ))
    assert (board.size == (3, 2))
  }

  it ("apply") {
    assert(exampleBoard(0) == List(
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

class SudokuXBoardSpec extends FunSpec {
    val exampleBoard = SudokuXBoard(List(
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

  it ("SudokuXBoard instance should be initialized") {
    SudokuXBoard(List(
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

  it ("centralReplacement") {
    val board = exampleBoard.centralReplacement
    assert(board(1) == List(
      SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
      SudokuCell(None), SudokuCell(None), SudokuCell(Some(7)),
      SudokuCell(Some(4)), SudokuCell(Some(8)), SudokuCell(None)
    ))
    assert(board(3) == List(
      SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
      SudokuCell(None), SudokuCell(Some(2)), SudokuCell(Some(9)),
      SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8))
    ))
  }

  describe ("edgeReplacement") {
    it ("edgeReplacement1") {
      val sigma = Map(0 -> 0, 1 -> 1, 2 -> 2)
      assert(exampleBoard.edgeReplacement(sigma) == exampleBoard)
    }

    // it ("edgeReplacement2") {
    //   val sigma = Map(0 -> 0, 1 -> 2, 2 -> 1)
    //   val resBoard = SudokuXBoard(List(
    //     List(
    //       SudokuCell(None), SudokuCell(None), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(None),
    //     ),
    //     List(
    //       SudokuCell(Some(3)), SudokuCell(Some(8)), SudokuCell(None),
    //       SudokuCell(Some(7)), SudokuCell(None), SudokuCell(None),
    //       SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
    //     ),
    //     List(
    //       SudokuCell(Some(5)), SudokuCell(Some(7)), SudokuCell(Some(6)),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(4)),
    //       SudokuCell(Some(8)), SudokuCell(None), SudokuCell(None),
    //     ),
    //
    //     List(
    //       SudokuCell(None), SudokuCell(Some(8)), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(3)),
    //       SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(1)),
    //     ),
    //     List(
    //       SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(2)),
    //       SudokuCell(Some(1)), SudokuCell(Some(7)), SudokuCell(None),
    //       SudokuCell(Some(9)), SudokuCell(Some(4)), SudokuCell(None),
    //     ),
    //     List(
    //       SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
    //       SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8)),
    //     ),
    //
    //     List(
    //       SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(4)),
    //       SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
    //       SudokuCell(Some(1)), SudokuCell(Some(5)), SudokuCell(None),
    //     ),
    //     List(
    //       SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
    //       SudokuCell(Some(8)), SudokuCell(None), SudokuCell(Some(2)),
    //       SudokuCell(None), SudokuCell(Some(2)), SudokuCell(Some(6)),
    //     ),
    //     List(
    //       SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(6)),
    //       SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
    //       SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(7)),
    //     ),
    //   ))
    //
    //   assert(exampleBoard.edgeReplacement(sigma) == resBoard)
    // }

    // it ("edgeReplacement3") {
    //   val sigmaInv = Map(0 -> 2, 1 -> 0, 2 -> 1)
    //   val board = exampleBoard.edgeReplacement(sigmaInv)
    //
    //   assert(board(0, 0) == SudokuCell(Some(8)))
    //   assert(board(1, 1) == SudokuCell(None))
    //   assert(board(2, 2) == SudokuCell(Some(6)))
    //
    //   assert(board(8, 0) == SudokuCell(None))
    //   assert(board(7, 1) == SudokuCell(None))
    //   assert(board(6, 2) == SudokuCell(Some(8)))
    //
    //   assert(board(0, 8) == SudokuCell(Some(1)))
    //   assert(board(1, 7) == SudokuCell(Some(2)))
    //   assert(board(2, 6) == SudokuCell(Some(4)))
    //
    //   assert(board(6, 6) == SudokuCell(Some(1)))
    //   assert(board(7, 7) == SudokuCell(Some(7)))
    //   assert(board(8, 8) == SudokuCell(Some(2)))
    // }
  }

  describe ("normalize") {
    it ("normalize1") {
      val resBoard = SudokuXBoard(List(
        List(
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(1)),
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
        ),
        List(
          SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(None),
          SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
          SudokuCell(Some(5)), SudokuCell(Some(6)), SudokuCell(None),
        ),
        List(
          SudokuCell(Some(7)), SudokuCell(Some(4)), SudokuCell(Some(6)),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(5)),
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
        ),

        List(
          SudokuCell(None), SudokuCell(Some(6)), SudokuCell(None),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(2)),
          SudokuCell(Some(3)), SudokuCell(None), SudokuCell(Some(8)),
        ),
        List(
          SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(9)),
          SudokuCell(Some(8)), SudokuCell(Some(4)), SudokuCell(None),
          SudokuCell(Some(1)), SudokuCell(Some(5)), SudokuCell(None),
        ),
        List(
          SudokuCell(Some(5)), SudokuCell(None), SudokuCell(Some(2)),
          SudokuCell(Some(1)), SudokuCell(Some(9)), SudokuCell(None),
          SudokuCell(None), SudokuCell(Some(4)), SudokuCell(Some(6)),
        ),

        List(
          SudokuCell(Some(6)), SudokuCell(Some(1)), SudokuCell(Some(8)),
          SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(None),
          SudokuCell(Some(9)), SudokuCell(Some(7)), SudokuCell(None),
        ),
        List(
          SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(7)),
          SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(9)),
          SudokuCell(None), SudokuCell(Some(8)), SudokuCell(Some(3)),
        ),
        List(
          SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(Some(3)),
          SudokuCell(None), SudokuCell(Some(8)), SudokuCell(Some(7)),
          SudokuCell(Some(6)), SudokuCell(Some(1)), SudokuCell(Some(4)),
        ),
      ))

      assert(exampleBoard.normalize == resBoard)
    }

    it ("normalize2") {
      val board = SudokuXBoard.parse(
        "......................................." +
        "1...23.4..5....6.7.8..........4.2..2....5.").get
      assert(board.normalize.toString == board.toString)
    }

    it ("normalize3") {
      val board = SudokuXBoard.parse(
        "......................................." +
        "2...34.5..6....7.8.9..........5.3..3....6.").get
      val resBoard = SudokuXBoard.parse(
        "......................................." +
        "1...23.4..5....6.7.8..........4.2..2....5.").get
      assert(board.normalize == resBoard)
    }
  }

  describe ("candidate") {
    it ("candidates1") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.candidates(2, 1) == Set(9))
    }

    it ("candidates2") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.candidates(8, 7) == Set(2, 3, 4, 5, 6, 7, 9))
    }

    it ("candidates3") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.candidates(7, 6) == Set(2, 3, 4, 5, 6, 8, 9))
    }

    it ("candidates4") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.candidates(3, 3) == Set(4, 5, 6, 7, 8, 9))
    }

    it ("candidates5") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.candidates(1, 7) == Set(1, 3, 4, 5, 6, 8, 9))
    }
  }

  describe ("solveNext1") {
    it ("横方向") {
      val board = SudokuXBoard.parse(
        "........." +
        "12.345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      val resBoard = SudokuXBoard.parse(
        "........." +
        "129345678" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........1"
      ).get
      assert(board.solveNext1 == resBoard)
    }

    it ("縦方向") {
      val board = SudokuXBoard.parse(
        "...1....." +
        "...2....." +
        "........." +
        "...6....." +
        "...7....." +
        "...8....." +
        "...9....." +
        "...3....." +
        "...5....1"
      ).get
      val resBoard = SudokuXBoard.parse(
        "...1....." +
        "...2....." +
        "...4....." +
        "...6....." +
        "...7....." +
        "...8....." +
        "...9....." +
        "...3....." +
        "...5....1"
      ).get
      assert(board.solveNext1 == resBoard)
    }

    it ("セル内") {
      val board = SudokuXBoard.parse(
        "........." +
        "........." +
        "........." +
        "......325" +
        "......8.1" +
        "......679" +
        "........." +
        "........." +
        ".......1."
      ).get
      val resBoard = SudokuXBoard.parse(
        "........." +
        "........." +
        "........." +
        "......325" +
        "......841" +
        "......679" +
        "........." +
        "........." +
        ".......1."
      ).get
      assert(board.solveNext1 == resBoard)
    }

    it ("対角線上") {
      val board = SudokuXBoard.parse(
        "........." +
        ".3......." +
        "..5......" +
        "...7....." +
        "....9...." +
        ".....2..." +
        "......6.." +
        ".......4." +
        ".......18"
      ).get
      val resBoard = SudokuXBoard.parse(
        "1........" +
        ".3......." +
        "..5......" +
        "...7....." +
        "....9...." +
        ".....2..." +
        "......6.." +
        ".......4." +
        ".......18"
      ).get
      assert(board.solveNext1 == resBoard)
    }

    it ("逆対角線上") {
      val board = SudokuXBoard.parse(
        "........." +
        ".......2." +
        "......5.." +
        ".....8..." +
        "....4...." +
        "...7....." +
        "..9......" +
        ".3......." +
        "1......7."
      ).get
      val resBoard = SudokuXBoard.parse(
        "........6" +
        ".......2." +
        "......5.." +
        ".....8..." +
        "....4...." +
        "...7....." +
        "..9......" +
        ".3......." +
        "1......7."
      ).get
      assert(board.solveNext1 == resBoard)
    }
  }

  it ("solveNext2") {
    val board = SudokuXBoard.parse(
      "123456..." +
      "........." +
      "........." +
      "........." +
      "........." +
      "........." +
      "......7.." +
      "......8.." +
      "........2"
    ).get
    val resBoard = SudokuXBoard.parse(
      "1234569.." +
      "........." +
      "........." +
      "........." +
      "........." +
      "........." +
      "......7.." +
      "......8.." +
      "........2"
    ).get
    assert(board.solveNext2 == resBoard)
  }

  describe ("solveNext3") {
    it ("横方向") {
      val board = SudokuXBoard.parse(
        "...456139" +
        "........." +
        "7........" +
        "..8......" +
        "........." +
        "8........" +
        "..7...7.." +
        "......8.." +
        "........2"
      ).get
      val resBoard = SudokuXBoard.parse(
        ".8.456139" +
        "........." +
        "7........" +
        "..8......" +
        "........." +
        "8........" +
        "..7...7.." +
        "......8.." +
        "........2"
      ).get
      assert(board.solveNext3 == resBoard)
    }
  }

  it ("縦方向") {
    val board = SudokuXBoard.parse(
      "..7..8..." +
      "....8..7." +
      "........." +
      "4........" +
      "5........" +
      "6........" +
      "1........" +
      "3........" +
      "9........"
    ).get
    val resBoard = SudokuXBoard.parse(
      "..7..8..." +
      "....8..7." +
      "8........" +
      "4........" +
      "5........" +
      "6........" +
      "1........" +
      "3........" +
      "9........"
    ).get
    assert(board.solveNext3 == resBoard)
  }

  describe ("solveNext") {
    it ("count") {
      val board = SudokuXBoard.parse(
        "............................................1..234.....5...6......72..6.8.4.....5"
      ).get
      assert(board.solveNext.count == board.count + 1)
    }
  }
}
