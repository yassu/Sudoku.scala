package sudoku

case class SudokuCell(value: Option[Int]) {
  override def toString: String = value match {
    case Some(n) => n.toString
    case None => "."
  }
}

case class Board(cells: List[List[SudokuCell]]) {
  override def toString: String = cells.map(cellRow => cellRow.mkString("")).mkString("")
}
