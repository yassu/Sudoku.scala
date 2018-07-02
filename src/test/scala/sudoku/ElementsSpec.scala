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
}
