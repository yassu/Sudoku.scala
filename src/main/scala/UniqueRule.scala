package sudoku

sealed abstract class UniqueRule {
  def uniquePositions: Seq[(Int, Int)]
  def onLine(x: Int, y: Int): Boolean =
    uniquePositions.contains((x, y))
}

object UniqueRule {
  val sudokuXRules: Seq[UniqueRule] =
    (0 until 9).map(new OneColumnUniqueRule(_)) ++
    (0 until 9).map(new OneRowUniqueRule(_)) ++
    (for (yCell <- (0 until 3); xCell <- (0 until 3)) yield new OneCellUniqueRule(xCell, yCell)) :+
    (new DiagonalUniqueRule) :+
    (new InvDiagonalUniqueRule)
  val sudokuRules: Seq[UniqueRule] =
    (0 until 9).map(new OneColumnUniqueRule(_)) ++
    (0 until 9).map(new OneRowUniqueRule(_)) ++
    (for (yCell <- (0 until 3); xCell <- (0 until 3)) yield new OneCellUniqueRule(xCell, yCell))
}

private[sudoku] class OneColumnUniqueRule(colNumber: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (y <- (0 until 9)) yield (colNumber, y)
}

private[sudoku] class OneRowUniqueRule(rowNumber: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (x <- (0 until 9)) yield (x, rowNumber)
}

private[sudoku] class OneCellUniqueRule(xCellNumber: Int, yCellNumber: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (
      y <- (3 * yCellNumber until 3 * yCellNumber + 3);
      x <- (3 * xCellNumber until 3 * xCellNumber + 3)) yield (x, y)
}

private[sudoku] class DiagonalUniqueRule extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until 9)) yield (j, j)
}

private[sudoku] class InvDiagonalUniqueRule extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until 9)) yield (8 - j, j)
}
