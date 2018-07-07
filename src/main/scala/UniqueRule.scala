package sudoku

sealed abstract class UniqueRule {
  def uniquePositions: Seq[(Int, Int)]
  def onLine(x: Int, y: Int): Boolean =
    uniquePositions.contains((x, y))
}

object UniqueRule {
  def sudokuXRules(size: Int): Seq[UniqueRule] =
    (0 until size).map(new OneColumnUniqueRule(_, size)) ++
    (0 until size).map(new OneRowUniqueRule(_, size)) ++
    (for (yCell <- (0 until MathUtil.sqrtInt(size)); xCell <- (0 until MathUtil.sqrtInt(size)))
      yield new OneCellUniqueRule(xCell, yCell, MathUtil.sqrtInt(size))) :+
    (new DiagonalUniqueRule(size)) :+
    (new InvDiagonalUniqueRule(size))
  def sudokuRules(size: Int): Seq[UniqueRule] =
    (0 until size).map(new OneColumnUniqueRule(_, size)) ++
    (0 until size).map(new OneRowUniqueRule(_, size)) ++
    (for (yCell <- (0 until MathUtil.sqrtInt(size)); xCell <- (0 until MathUtil.sqrtInt(size)))
      yield new OneCellUniqueRule(xCell, yCell, MathUtil.sqrtInt(size)))
}

private[sudoku] class OneColumnUniqueRule(colNumber: Int, size: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (y <- (0 until size)) yield (colNumber, y)
}

private[sudoku] class OneRowUniqueRule(rowNumber: Int, size: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (x <- (0 until size)) yield (x, rowNumber)
}

private[sudoku] class OneCellUniqueRule(xCellNumber: Int, yCellNumber: Int, cellSize: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (
      y <- (cellSize * yCellNumber until cellSize * yCellNumber + cellSize);
      x <- (cellSize * xCellNumber until cellSize * xCellNumber + cellSize)) yield (x, y)
}

private[sudoku] class DiagonalUniqueRule(size: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until size)) yield (j, j)
}

private[sudoku] class InvDiagonalUniqueRule(size: Int) extends UniqueRule {
  val uniquePositions: Seq[(Int, Int)] =
    for (j <- (0 until size)) yield (size  - j - 1, j)
}
