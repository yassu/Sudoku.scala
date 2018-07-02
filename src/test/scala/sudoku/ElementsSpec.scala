package sudoku

import org.scalatest.FunSpec

class SudokuBoardSpec extends FunSpec {
  it ("SudokuBoard should be initialized.") {
    SudokuBoard(Seq(
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
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
