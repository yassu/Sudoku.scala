package sudoku

import org.scalatest.FunSpec

class SudokuBoardSpec extends FunSpec {
    val exampleBoard = SudokuBoard(Seq(
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

  it ("SudokuBoard should be initialized.") {
    SudokuBoard(Seq(
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
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

  it ("easy problem") {
    val easyProblem = SudokuBoard.parse(
      "..8.5...." +
      "......9.6" +
      "12..7...." +
      "8....6.95" +
      "4.3....12" +
      "6153.94.7" +
      "289467.53" +
      "751.93.6." +
      "3.4.1827.",
      9
      ).get

    val ans = SudokuBoard.parse(
      "938652741" +
      "547831926" +
      "126974538" +
      "872146395" +
      "493785612" +
      "615329487" +
      "289467153" +
      "751293864" +
      "364518279",
      9
   ).get
   val sols = easyProblem.solve
   assert(sols.size == 1)
   val sol = sols.head
   assert(sols == Set(ans))
  }

  it ("hard problem") {
    val board = SudokuBoard.parse(
      "..53....." +
      "8......2." +
      ".7..1.5.." +
      "4....53.." +
      ".1..7...6" +
      "..32...8." +
      ".6.5....9" +
      "..4....3." +
      ".....97..",
      9
    ).get
    val sols = board.solve
    val sol = sols.head
    assert(sols.size == 1 && sol.count == 81 && sol.ensure)
  }
}
