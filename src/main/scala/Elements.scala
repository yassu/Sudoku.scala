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
  var _candidates: mutable.Map[(Int, Int), Set[Int]] = mutable.Map()

  if (! cells.forall(cellRow => cellRow.size == sizeOne)) {
    throw new IllegalArgumentException("Cell size is strange.")
  }

  def candidates(x: Int, y: Int): Set[Int] =
    if (_candidates.keySet.contains((x, y))) {
      _candidates((x, y))
    }
    else if (this(x, y).isDefined) {
      Set(this(x, y).value.get)
    }
    else {
      var s = mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      UniqueLineRules.sudokuXRules
        .filter(_.onLine(x, y))
        .foreach(rule =>
          {
             rule
              .uniquePositions
              .map(t => this(t._1, t._2))
              .foreach(cell => cell.value match {
                  case Some(n) => s -= n
                  case None =>
                }
              )
          }
        )

      val res = s.toSet
      _candidates((x, y)) = res
      res
    }


  def solveNext1: Board = {
    rules.foreach(rule => {
      val positions = rule.uniquePositions.filter(t => this(t._1, t._2).isDefined)
        if (positions.size == 8) {
          val pos = rule.uniquePositions.filter(! positions.contains(_)).head
          val values = positions.map(t => this(t._1, t._2).value.get).toSet
          val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
          return this.changeBoard(pos._1, pos._2, SudokuCell(Some(sol)))
        }
      }
    )

    this
  }

  def solveNext2: Board = {
    for (y <- (0 until 9)) {
      for (x <- (0 until 9)) {
        val candidates = this.candidates(x, y)
        if (! this(x, y).isDefined && candidates.size == 1) {
          val value = candidates.head
          return this.changeBoard(x, y, SudokuCell(Some(value)))
        }
      }
    }
    this
  }
}
