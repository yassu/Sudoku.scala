package sudoku

import scala.collection.mutable
import scala.math.Ordering

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

  def countMap: Map[Int, Int] =
    (1 to 9).map(j => j -> this.toString.count(_ == j.toString.toCharArray.head)).toMap

  def row(y: Int): List[SudokuCell] = this(y)
  def col(x: Int): List[SudokuCell] = (0 until 9).map(this(x, _)).toList
  def apply(x: Int, y: Int): SudokuCell = cells(y)(x)
  def apply(y: Int): List[SudokuCell] = (
    for (x <- (0 until 9)) yield this(x, y)
  ).toList
  def toSudokuXBoard: SudokuXBoard = SudokuXBoard(cells)
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
  def representative(fs: Set[SudokuXBoard => SudokuXBoard]): SudokuXBoard =
    fs.map(f => f(this)).min

  override def rotate: SudokuXBoard =
    super.rotate.toSudokuXBoard

  override def flip: SudokuXBoard =
    super.flip.toSudokuXBoard

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

  def edgeReplacement(sigmaInv: Map[Int, Int]): SudokuXBoard = {
    val board = this.map(
      ((x: Int, y: Int) =>
        if (0 <= x && x < 3) this(sigmaInv(x), y)
        else if (6 <= x && x < 9) this(8 - sigmaInv(8 - x), y)
        else this(x, y)
      )
    )
    board.map (
      ((x: Int, y: Int) =>
        if (0 <= y && y < 3) board(x, sigmaInv(y))
        else if (6 <= y && y < 9) board(x, 8 - sigmaInv(8 - y))
        else board(x, y)
      )
    )
  }

  def normalize: SudokuXBoard = {
    def normalizeMap: Map[Int, Int] = {
      var map = mutable.Map[Int, Int]()
      var count = 0
      for (y <- (0 until 9)) {
        for (x <- (0 until 9)) {
          this(x, y).value match {
            case Some(n) => {
              if (! map.keys.toList.contains(n)) {
                count += 1
                map(n) = count
              }
            }
            case None =>
          }
        }
      }

      if (map.size < 8)
        throw new IllegalArgumentException("Count of " + this.toString + " numbers < 8")
      else if (map.size == 8) {
        val nonExistedNumber = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- map.keys.toSet).head
        map(nonExistedNumber) = 9
      }

      map.toMap
    }

    val nMap = normalizeMap
    this.map(
      (x: Int, y: Int) => this(x, y).value match {
        case Some(n) => SudokuCell(Some(nMap(n)))
        case None => SudokuCell(None)
      }
    )
  }

  def solveNext1: SudokuXBoard = {
    // 横方向
    for (y0 <- (0 until 9)) {
      val cal = this(y0).filter(_.isDefined)
      if (cal.size == 8) {
        val x0 = (0 until 9).filter(! this(_, y0).isDefined).head
        val values = cal.filter(_.isDefined).map(_.value.get).toSet
        val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
        return this.map(
          (x: Int, y: Int) => if (x == x0 && y == y0) SudokuCell(Some(sol)) else this(x, y)
        )
      }
    }
    this
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
  def equivalentTransformations: Set[SudokuXBoard => SudokuXBoard] = {
    val s3 = Set(
      Map(0 -> 0, 1 -> 1, 2 -> 2),
      Map(0 -> 0, 1 -> 2, 2 -> 1),
      Map(0 -> 1, 1 -> 0, 2 -> 2),
      Map(0 -> 1, 1 -> 2, 2 -> 0),
      Map(0 -> 2, 1 -> 1, 2 -> 0),
      Map(0 -> 2, 1 -> 0, 2 -> 1)
    )

    val fs = FuncUtil.funcProducts(List(
      ((b: SudokuXBoard) => b.centralReplacement, 1),
      ((b: SudokuXBoard) => b.rotate, 3),
      ((b: SudokuXBoard) => b.flip, 1),
    )).toSet

    for (f <- fs; g <- s3) yield f.compose((b: SudokuXBoard) => b.edgeReplacement(g)) andThen
      (b => b.normalize)
  }

  def apply(cells: List[List[SudokuCell]]): SudokuXBoard =
    new SudokuXBoard(cells)

  def parse(s: String): Option[SudokuXBoard] = {
    val ok =
      s.size == 81 &&
      s.forall(Set('1', '2', '3', '4', '5', '6', '7', '8', '9', '.').contains(_))

    if (ok)
      Some(SudokuXBoard(
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

  implicit val ordering: Ordering[SudokuXBoard] = Ordering.by(b => b.toString)
}
