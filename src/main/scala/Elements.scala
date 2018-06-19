package sudoku

case class SudokuCell(value: Option[Int]) {
  override def toString: String = value match {
    case Some(n) => n.toString
    case None => "."
  }
}

object SudokuCell {
  def parse(c: Char): SudokuCell =
    if (Set('1', '2', '3', '4', '5', '6', '7', '8', '9').contains(c)) SudokuCell(Some(c.asDigit))
    else SudokuCell(None)
}

case class Board(cells: List[List[SudokuCell]]) {
  val countSet: Set[Int] = countMap.values.toSet

  def countMap: Map[Int, Int] = {
    val containsInt = (row: List[SudokuCell], n: Int) =>
      if (row.contains(SudokuCell(Some(n)))) 1 else 0

    if (cells.isEmpty)
      Map(
        1 -> 0,
        2 -> 0,
        3 -> 0,
        4 -> 0,
        5 -> 0,
        6 -> 0,
        7 -> 0,
        8 -> 0,
        9 -> 0,
      )
    else {
      val beforeMap = Board(cells.drop(1)).countMap
      val cellRow = cells.head
      Map (
        1 -> (containsInt(cellRow, 1) + beforeMap(1)),
        2 -> (containsInt(cellRow, 2) + beforeMap(2)),
        3 -> (containsInt(cellRow, 3) + beforeMap(3)),
        4 -> (containsInt(cellRow, 4) + beforeMap(4)),
        5 -> (containsInt(cellRow, 5) + beforeMap(5)),
        6 -> (containsInt(cellRow, 6) + beforeMap(6)),
        7 -> (containsInt(cellRow, 7) + beforeMap(7)),
        8 -> (containsInt(cellRow, 8) + beforeMap(8)),
        9 -> (containsInt(cellRow, 9) + beforeMap(9)),
      )
    }
  }
  override def toString: String = cells.map(cellRow => cellRow.mkString("")).mkString("")
}

object Board {
  def parse(s: String): Option[Board] = {
    val ok =
      s.size == 81 &&
      s.forall(Set('1', '2', '3', '4', '5', '6', '7', '8', '9', '.').contains(_))

    if (ok)
      Some(Board(
      (
        for (y <- (0 until 9)) yield
        (
          for (x <- (0 until 9)) yield SudokuCell.parse(s(9 * y + x))
        ).toList
      ).toList
    ))
    else
      None
  }
}
