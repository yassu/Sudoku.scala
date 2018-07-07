package sudoku.sudokux

import org.scalatest.FunSpec
import sudoku.{SudokuCell}

class SudokuXBoardSpec extends FunSpec {
    val exampleBoard = SudokuXBoard(Seq(
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

  it ("SudokuXBoard instance should be initialized") {
    SudokuXBoard(Seq(
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

  it ("centralReplacement") {
    val board = exampleBoard.centralReplacement
    assert(board(1) == Seq(
      SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
      SudokuCell(None), SudokuCell(None), SudokuCell(Some(7)),
      SudokuCell(Some(4)), SudokuCell(Some(8)), SudokuCell(None)
    ))
    assert(board(3) == Seq(
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
    //   val resBoard = SudokuXBoard(Seq(
    //     Seq(
    //       SudokuCell(None), SudokuCell(None), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(None),
    //     ),
    //     Seq(
    //       SudokuCell(Some(3)), SudokuCell(Some(8)), SudokuCell(None),
    //       SudokuCell(Some(7)), SudokuCell(None), SudokuCell(None),
    //       SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
    //     ),
    //     Seq(
    //       SudokuCell(Some(5)), SudokuCell(Some(7)), SudokuCell(Some(6)),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(4)),
    //       SudokuCell(Some(8)), SudokuCell(None), SudokuCell(None),
    //     ),
    //
    //     Seq(
    //       SudokuCell(None), SudokuCell(Some(8)), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(None), SudokuCell(Some(3)),
    //       SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(1)),
    //     ),
    //     Seq(
    //       SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(2)),
    //       SudokuCell(Some(1)), SudokuCell(Some(7)), SudokuCell(None),
    //       SudokuCell(Some(9)), SudokuCell(Some(4)), SudokuCell(None),
    //     ),
    //     Seq(
    //       SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
    //       SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(None),
    //       SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8)),
    //     ),
    //
    //     Seq(
    //       SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(4)),
    //       SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
    //       SudokuCell(Some(1)), SudokuCell(Some(5)), SudokuCell(None),
    //     ),
    //     Seq(
    //       SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
    //       SudokuCell(Some(8)), SudokuCell(None), SudokuCell(Some(2)),
    //       SudokuCell(None), SudokuCell(Some(2)), SudokuCell(Some(6)),
    //     ),
    //     Seq(
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
      val resBoard = SudokuXBoard(Seq(
        Seq(
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(1)),
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
        ),
        Seq(
          SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(None),
          SudokuCell(Some(4)), SudokuCell(None), SudokuCell(None),
          SudokuCell(Some(5)), SudokuCell(Some(6)), SudokuCell(None),
        ),
        Seq(
          SudokuCell(Some(7)), SudokuCell(Some(4)), SudokuCell(Some(6)),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(5)),
          SudokuCell(None), SudokuCell(None), SudokuCell(None),
        ),

        Seq(
          SudokuCell(None), SudokuCell(Some(6)), SudokuCell(None),
          SudokuCell(None), SudokuCell(None), SudokuCell(Some(2)),
          SudokuCell(Some(3)), SudokuCell(None), SudokuCell(Some(8)),
        ),
        Seq(
          SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(9)),
          SudokuCell(Some(8)), SudokuCell(Some(4)), SudokuCell(None),
          SudokuCell(Some(1)), SudokuCell(Some(5)), SudokuCell(None),
        ),
        Seq(
          SudokuCell(Some(5)), SudokuCell(None), SudokuCell(Some(2)),
          SudokuCell(Some(1)), SudokuCell(Some(9)), SudokuCell(None),
          SudokuCell(None), SudokuCell(Some(4)), SudokuCell(Some(6)),
        ),

        Seq(
          SudokuCell(Some(6)), SudokuCell(Some(1)), SudokuCell(Some(8)),
          SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(None),
          SudokuCell(Some(9)), SudokuCell(Some(7)), SudokuCell(None),
        ),
        Seq(
          SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(7)),
          SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(9)),
          SudokuCell(None), SudokuCell(Some(8)), SudokuCell(Some(3)),
        ),
        Seq(
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

  describe ("ensure") {
    it ("横方向") {
      val board = SudokuXBoard.parse(
        "........." +
        "123456789" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........."
      ).get
      assert(board.ensure)
    }

    it ("横方向2") {
      val board = SudokuXBoard.parse(
        "........." +
        "123456781" +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........." +
        "........."
      ).get
      assert(! board.ensure)
    }

    it ("縦方向") {
      val board = SudokuXBoard.parse(
        "...5....." +
        "...6....." +
        "...7....." +
        "...8....." +
        "...9....." +
        "...1....." +
        "...2....." +
        "...3....." +
        "...4....."
      ).get
      assert(board.ensure)
    }

    it ("縦方向2") {
      val board = SudokuXBoard.parse(
        "...5....." +
        "...6....." +
        "...7....." +
        "...8....." +
        "...9....." +
        "...1....." +
        "...2....." +
        "...3....." +
        "...8....."
      ).get
      assert(! board.ensure)
    }

    it ("セル内") {
      val board = SudokuXBoard.parse(
        "........." +
        "........." +
        "........." +
        "......671" +
        "......234" +
        "......895" +
        "........." +
        "........." +
        "........."
      ).get
      assert(board.ensure)
    }

    it ("セル内2") {
      val board = SudokuXBoard.parse(
        "........." +
        "........." +
        "........." +
        "......671" +
        "......234" +
        "......595" +
        "........." +
        "........." +
        "........."
      ).get
      assert(! board.ensure)
    }

    it ("対角線上") {
      val board = SudokuXBoard.parse(
        "1........" +
        ".2......." +
        "..3......" +
        "...4....." +
        "....5...." +
        ".....6..." +
        "......7.." +
        ".......8." +
        "........9"
      ).get
      assert(board.ensure)
    }

    it ("対角線上2") {
      val board = SudokuXBoard.parse(
        "1........" +
        ".2......." +
        "..3......" +
        "...4....." +
        "....5...." +
        ".....6..." +
        "......2.." +
        ".......8." +
        "........9"
      ).get
      assert(! board.ensure)
    }

    it ("逆対角線") {
      val board = SudokuXBoard.parse(
        "........3" +
        ".......1." +
        "......5.." +
        ".....2..." +
        "....4...." +
        "...6....." +
        "..9......" +
        ".8......." +
        "7........"
      ).get
      assert(board.ensure)
    }

    it ("逆対角線2") {
      val board = SudokuXBoard.parse(
        "........3" +
        ".......1." +
        "......5.." +
        ".....2..." +
        "....4...." +
        "...6....." +
        "..2......" +
        ".8......." +
        "7........"
      ).get
      assert(! board.ensure)
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

  it ("solve") {
      val board = SudokuXBoard.parse(
        "............................................1..2345...56.......1......7....8..23."
      ).get
      val sols = board.solve
      assert(sols.size == 1)
      assert(sols.head.count == 81)
  }
}
