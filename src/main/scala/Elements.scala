package sudoku

import scala.collection.mutable
import scala.math.Ordering
import sudoku.UniqueLineRules
import sudokux.SudokuXBoard

case class SudokuCell(value: Option[Int]) {
  def isDefined: Boolean = value match {
    case Some(n) => true
    case None => false
  }
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

case class Board(cells: Seq[Seq[SudokuCell]]) {
  val countSet: Set[Int] = countMap.values.toSet

  def rotate: Board = this.map(
    (x: Int, y: Int) => this(y, 8 - x)
  )

  def flip: Board = this.map(
    (x: Int, y: Int) => this(8 - x, y)
  )

  def changeBoard(x0: Int, y0: Int, cell: SudokuCell) = Board(
      (
        for (y <- (0 until 9)) yield
        (
          for (x <- (0 until 9)) yield
            if (x == x0 && y == y0) cell
            else this(x, y)
        ).toSeq
      ).toSeq
    )

  def map(f: (Int, Int) => SudokuCell): Board = Board(
    (
      for (y <- (0 until 9)) yield
      (
        for (x <- (0 until 9)) yield f(x, y)
      ).toSeq
    ).toSeq
  )

  def countMap: Map[Int, Int] =
    (1 to 9).map(j => j -> this.toString.count(_ == j.toString.toCharArray.head)).toMap

  def row(y: Int): Seq[SudokuCell] = this(y)
  def col(x: Int): Seq[SudokuCell] = (0 until 9).map(this(x, _)).toSeq
  def size: (Int, Int) = (this.cells(0).size, this.cells.size)
  def apply(x: Int, y: Int): SudokuCell = cells(y)(x)
  def apply(y: Int): Seq[SudokuCell] = (
    for (x <- (0 until 9)) yield this(x, y)
  ).toSeq
  def toSudokuXBoard: SudokuXBoard = SudokuXBoard(cells)
  def toPrettyString: String = cells.map(cellRow => cellRow.mkString("")).mkString("\n")
  override def toString: String = cells.map(cellRow => cellRow.mkString("")).mkString("")
}

object Board {
  def parse(s: String, size: Int): Option[Board] = {
    val ok =
      s.size == size * size &&
      s.forall(c => (1 to size).map(_.toString).contains(c.toString) || c == '.')

    if (ok)
      Some(Board(
      (
        for (y <- (0 until size)) yield
        (
          for (x <- (0 until size)) yield SudokuCell.parse(s(size * y + x))
        ).toSeq
      ).toSeq
    ))
    else
      None
  }

  def numbers(size: Int): Set[Int] = (1 to size).toSet
  def numberStrings(size: Int): Set[String] = (1 to size).map(_.toString).toSet
}

abstract class CommonSudokuBoard(cells: Seq[Seq[SudokuCell]]) extends Board(cells) {
  if (cells.isEmpty) {
    throw new IllegalArgumentException("Cell of squareboard shouble not be empty.")
  }

  if (cells.head.size != cells.size) {
    throw new IllegalArgumentException("Number of col should be number of row.")
  }

  val sizeOne: Int = cells.head.size
  val rules: Seq[UniqueLineRules]

  if (! cells.forall(cellRow => cellRow.size == sizeOne)) {
    throw new IllegalArgumentException("Cell size is strange.")
  }
}
