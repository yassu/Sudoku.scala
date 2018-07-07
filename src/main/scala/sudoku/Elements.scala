package sudoku

class SudokuBoard(cells: Seq[Seq[SudokuCell]]) extends CommonSudokuBoard(cells) {
  val rules = UniqueRule.sudokuRules(this.sizeOne)
  def solve: Set[SudokuBoard] =
    CommonSudokuBoard.solve(this, _.toSudokuBoard)
}

object SudokuBoard {
  def apply(cells: Seq[Seq[SudokuCell]]): SudokuBoard = new SudokuBoard(cells)
  def parse(s: String): Option[SudokuBoard] = Board.parse(s) match {
    case Some(board) => Some(board.toSudokuBoard)
    case None => None
  }
}
