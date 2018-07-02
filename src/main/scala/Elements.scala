package sudoku

import scala.collection.mutable
import scala.math.Ordering
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
    (x: Int, y: Int) => this(y, size._1 - x - 1)
  )

  def flip: Board = this.map(
    (x: Int, y: Int) => this(size._1 - x - 1, y)
  )

  def changeBoard(x0: Int, y0: Int, cell: SudokuCell) = Board(
      (
        for (y <- (0 until size._2)) yield
        (
          for (x <- (0 until size._1)) yield
            if (x == x0 && y == y0) cell
            else this(x, y)
        ).toSeq
      ).toSeq
    )

  def map(f: (Int, Int) => SudokuCell): Board = Board(
    (
      for (y <- (0 until size._2)) yield
      (
        for (x <- (0 until size._1)) yield f(x, y)
      ).toSeq
    ).toSeq
  )

  def countMap: Map[Int, Int] =
    (1 to 9).map(j => j -> this.toString.count(_ == j.toString.toCharArray.head)).toMap

  def diff(board: Board, onlyDefined: Boolean =false): Seq[((Int, Int), (SudokuCell, SudokuCell))] =
    (for (x <- (0 until size._1); y <- (0 until size._2)) yield (x, y))
    .filter(pos => (! onlyDefined ||
      this(pos._1, pos._2).isDefined && board(pos._1, pos._2).isDefined))
    .filter(pos => this(pos._1, pos._2) != board(pos._1, pos._2))
    .map(pos => (pos, (this(pos._1, pos._2), board(pos._1, pos._2)))).toSeq

  def row(y: Int): Seq[SudokuCell] = this(y)
  def col(x: Int): Seq[SudokuCell] = (0 until size._2).map(this(x, _)).toSeq
  val size: (Int, Int) = (this.cells(0).size, this.cells.size)
  def count: Int = (for (y <- (0 until size._2); x <- (0 until size._1)) yield this(x, y)).count(_.isDefined)
  def apply(x: Int, y: Int): SudokuCell = cells(y)(x)
  def apply(y: Int): Seq[SudokuCell] = (
    for (x <- (0 until size._1)) yield this(x, y)
  ).toSeq
  def toSudokuBoard: SudokuBoard = SudokuBoard(cells)
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
  val rules: Seq[UniqueRule]
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
      var s = mutable.Set((1 to sizeOne): _*)
      rules
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
        if (positions.size == sizeOne - 1) {
          val pos = rule.uniquePositions.filter(! positions.contains(_)).head
          val values = positions.map(t => this(t._1, t._2).value.get).toSet
          val sol = (Set((1 to sizeOne): _*) -- values).head
          return this.changeBoard(pos._1, pos._2, SudokuCell(Some(sol)))
        }
      }
    )

    this
  }

  def solveNext2: Board = {
    for (y <- (0 until sizeOne)) {
      for (x <- (0 until sizeOne)) {
        val candidates = this.candidates(x, y)
        if (! this(x, y).isDefined && candidates.size == 1) {
          val value = candidates.head
          return this.changeBoard(x, y, SudokuCell(Some(value)))
        }
      }
    }
    this
  }

  def solveNext3: Board = {
    for (rule <- rules) {
      var numbers = Set((1 to sizeOne): _*) --
        rule.uniquePositions.map(pos => this(pos._1, pos._2)).filter(_.isDefined).map(_.value.get)
        .toSet
      for (number <- numbers) {
        val positions =
          rule.uniquePositions.filter(t => this.candidates(t._1, t._2).contains(number)).toSet
        if (positions.size == 1) {
            val position = positions.head
            return changeBoard(position._1, position._2, SudokuCell(Some(number)))
        }
      }
    }

    this
  }


  def solveNext: Board = {
    val board1 = this.solveNext1
    if (this != board1) {
      return board1
    }

    val board2 = this.solveNext2
    if (this != board2) {
      return board2
    }

    val board3 = this.solveNext3
    if (this != board3) {
      return board3
    }

    return this
  }

  def ensure: Boolean =
    rules.forall(
      rule => {
        val numbers = rule.uniquePositions.map(t => this(t._1, t._2)).filter(_.isDefined)
        numbers == numbers.distinct
      }
    )
}

object CommonSudokuBoard {
  def solve[T <: CommonSudokuBoard](board: T, f: Board => T): Set[T] = {
    def _solve(board: T): T = {
      val sol = f(board.solveNext)
      if (board == sol) {
        board
      }
      else {
        _solve(sol)
      }
    }

    val sol = _solve(board)
    val ok = sol.ensure

    if (! ok)
      Set()
    else if (sol.count == board.sizeOne * board.sizeOne)
      Set(sol)
    else {
      val position = (for (y <- (0 until board.sizeOne); x <- (0 until board.sizeOne)) yield (x, y))
        .filter(t => ! board(t._1, t._2).isDefined)
        .minBy(t => board.candidates(t._1, t._2).size)
      val candidates = board.candidates(position._1, position._2)
      candidates
        .map(n => board.map(
          (x: Int, y: Int) =>
            if (x == position._1 && y == position._2) SudokuCell(Some(n))
            else board(x, y)
        ))
        .map(b => CommonSudokuBoard.solve(f(b), f)).flatMap {x => x}
        .filter(_.ensure).toSet
    }
  }
}
