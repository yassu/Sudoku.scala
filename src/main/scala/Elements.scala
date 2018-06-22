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

  def rotate: Board = this.map(
    (x: Int, y: Int) => this(y, 8 - x)
  )

  def flip: Board = this.map(
    (x: Int, y: Int) => this(8 - x, y)
  )

  def map(f: (Int, Int) => SudokuCell): Board = Board(
    (
      for (y <- (0 until 9)) yield
      (
        for (x <- (0 until 9)) yield f(x, y)
      ).toList
    ).toList
  )

  def countMap: Map[Int, Int] = {
    val s = this.toString
    Map (
      1 -> s.count(_ == '1'),
      2 -> s.count(_ == '2'),
      3 -> s.count(_ == '3'),
      4 -> s.count(_ == '4'),
      5 -> s.count(_ == '5'),
      6 -> s.count(_ == '6'),
      7 -> s.count(_ == '7'),
      8 -> s.count(_ == '8'),
      9 -> s.count(_ == '9'),
    )
  }

  def apply(x: Int, y: Int): SudokuCell = cells(y)(x)
  def apply(y: Int): List[SudokuCell] = (
    for (x <- (0 until 9)) yield this(x, y)
  ).toList
  def toPrettyString: String = cells.map(cellRow => cellRow.mkString("")).mkString("\n")
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

class SudokuXBoard(cells: List[List[SudokuCell]]) extends Board(cells) {
  def centralReplacement: SudokuXBoard = {
    val board = this.map(
      ((x: Int, y: Int) =>
        if (2 < x && x < 6) this(8 - x, y)
        else this(x, y)
      ))
    board.map(
      ((x: Int, y: Int) =>
        if (2 < y && y < 6) board(x, 8 - y)
        else board(x, y)
      )
    )
  }

  override def map(f: (Int, Int) => SudokuCell): SudokuXBoard = SudokuXBoard(
    (
      for (y <- (0 until 9)) yield
      (
        for (x <- (0 until 9)) yield f(x, y)
      ).toList
    ).toList
  )
}

object SudokuXBoard {
  def apply(cells: List[List[SudokuCell]]): SudokuXBoard =
    new SudokuXBoard(cells)
}
