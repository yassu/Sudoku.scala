package sudoku

sealed abstract class UniqueLineRules {
  def uniquePositions: Seq[(Int, Int)]
  def onLine(x: Int, y: Int): Boolean =
    uniquePositions.contains((x, y))
}

object UniqueLineRules {
  def sudokuXRules: Seq[UniqueLineRules] =
    (0 until 9).map(new OneColumnRule(_)) ++
    (0 until 9).map(new OneRowRule(_)) ++
    (for (yCell <- (0 until 3); xCell <- (0 until 3)) yield new OneCellRule(xCell, yCell)) :+
    (new DiagonalLineRule) :+
    (new InvDiagonalLineRule)
}

private[sudoku] class OneColumnRule(colNumber: Int) extends UniqueLineRules {
  val uniquePositions: Seq[(Int, Int)] =
    for (y <- (0 until 9)) yield (colNumber, y)
}

private[sudoku] class OneRowRule(rowNumber: Int) extends UniqueLineRules {
  val uniquePositions: Seq[(Int, Int)] =
    for (x <- (0 until 9)) yield (x, rowNumber)
}

private[sudoku] class OneCellRule(xCellNumber: Int, yCellNumber: Int) extends UniqueLineRules {
  val uniquePositions: Seq[(Int, Int)] =
    for (
      y <- (3 * yCellNumber until 3 * yCellNumber + 3);
      x <- (3 * xCellNumber until 3 * xCellNumber + 3)) yield (x, y)
}

private[sudoku] class DiagonalLineRule extends UniqueLineRules {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until 9)) yield (j, j)
}

private[sudoku] class InvDiagonalLineRule extends UniqueLineRules {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until 9)) yield (8 - j, j)
}
