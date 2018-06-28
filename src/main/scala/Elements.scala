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
  var _candidates: mutable.Map[(Int, Int), Set[Int]] = mutable.Map()

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

  def candidates(x: Int, y: Int): Set[Int] = {
    if (_candidates.keySet.contains((x, y))) {
      return _candidates((x, y))
    }

    if (this(x, y).isDefined)
    {
      return Set(this(x, y).value.get)
    }

    var s = mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9)

    // 横方向
    for (cell <- this.row(y)) {
      cell.value match {
        case Some(n) => s -= n
        case None =>
      }
    }

    // 縦方向
    for (cell <- this.col(x)) {
      cell.value match {
        case Some(n) => s -= n
        case None =>
      }
    }

    // セル内
    for (x0 <- (x / 3 * 3 until x / 3 * 3 + 3)) {
      for (y0 <- (y / 3 * 3 until y / 3 * 3 + 3)) {
        val cell = this(x0, y0)
        cell.value match {
          case Some(n) => s -= n
          case None =>
        }
      }
    }

    // 対角線
    if (x == y) {
      for (j <- (0 until 9)) {
        val cell = this(j, j)
        cell.value match {
          case Some(n) => s -= n
          case None =>
        }
      }
    }

    // 逆対角線
    if (x + y == 8) {
      for (j <- (0 until 9)) {
        val cell = this(j, 8 - j)
        cell.value match {
          case Some(n) => s -= n
          case None =>
        }
      }
    }

    val res = s.toSet
    _candidates((x, y)) = res
    res
  }

  def solveNext1: SudokuXBoard = {
    // 横方向
    for (y0 <- (0 until 9)) {
      val row = this(y0).filter(_.isDefined)
      if (row.size == 8) {
        val x0 = (0 until 9).filter(! this(_, y0).isDefined).head
        val values = row.map(_.value.get).toSet
        val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
        return this.map(
          (x: Int, y: Int) => if (x == x0 && y == y0) SudokuCell(Some(sol)) else this(x, y)
        )
      }
    }

    // 縦方向
    for (x0 <- (0 until 9)) {
      val col = this.col(x0).filter(_.isDefined)
      if (col.size == 8) {
        val y0 = (0 until 9).filter(! this(x0, _).isDefined).head
        val values = col.map(_.value.get).toSet
        val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
        return this.map(
          (x: Int, y: Int) => if (x == x0 && y == y0) SudokuCell(Some(sol)) else this(x, y)
        )
      }
    }

    // セル内
    for (x <- (0 until 3)) {
      for (y <- (0 until 3)) {
        val cells =
          (for (x0 <- (3 * x until 3 * x + 3); y0 <- (3 * y until 3 * y + 3)) yield this(x0, y0))
          .filter(_.isDefined)
        if (cells.size == 8) {
          val pos =
            (for (x0 <- (3 * x until 3 * x + 3); y0 <- (3 * y until 3 * y + 3)) yield (x0, y0))
            .filter(t => ! this(t._1, t._2).isDefined).head
          val values = cells.map(_.value.get).toSet
          val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
          return this.map(
            (x: Int, y: Int) => if (x == pos._1 && y == pos._2) SudokuCell(Some(sol)) else this(x, y)
          )
        }
      }
    }

    // 対角線上
    val diagonalCells = (for (j <- (0 until 9)) yield this(j, j)).filter(_.isDefined)
    if (diagonalCells.size == 8) {
      val pos = (for (j <- (0 until 9)) yield (j, j)).filter(pos => ! this(pos._1, pos._2).isDefined).head
      val values = diagonalCells.map(_.value.get).toSet
      val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
      return this.map(
        (x: Int, y: Int) => if (x == pos._1 && y == pos._2) SudokuCell(Some(sol)) else this(x, y)
      )
    }

    // 逆対角線上
    val invDiagonalCells = (for (j <- (0 until 9)) yield this(j, 8 - j)).filter(_.isDefined)
    if (invDiagonalCells.size == 8) {
      val pos = (for (j <- (0 until 9))
        yield (j, 8 - j)).filter(pos => ! this(pos._1, pos._2).isDefined).head
      val values = invDiagonalCells.map(_.value.get).toSet
      val sol = (Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- values).head
      return this.map(
        (x: Int, y: Int) => if (x == pos._1 && y == pos._2) SudokuCell(Some(sol)) else this(x, y)
      )
    }

    this
  }

  def solveNext2: SudokuXBoard = {
    for (y <- (0 until 9)) {
      for (x <- (0 until 9)) {
        val candidates = this.candidates(x, y)
        if (! this(x, y).isDefined && this.candidates(x, y).size == 1) {
          val value = this.candidates(x, y).head
          return this.map(
            (x0: Int, y0: Int) => if (x == x0 && y == y0) SudokuCell(Some(value)) else this(x0, y0)
          )
        }
      }
    }
    this
  }

  def solveNext3: SudokuXBoard = {
    // 横方向で候補になっている場所が一つだけなら確定
    for (y <- (0 until 9)) {
      var numbers = Set(1, 2, 3, 4, 5, 6, 7, 8, 9) -- this(y).filter(_.isDefined).map(_.value.get).toSet
      for (number <- numbers) {
        val positions =
          (for (x <- (0 until 9)) yield (x, y)).filter(t => this.candidates(t._1, t._2).contains(number))
          .toSet
        if (positions.size == 1) {
          val position = positions.head
          return this.map(
            (x0: Int, y0: Int) =>
              if (x0 == position._1 && y0 == position._2) SudokuCell(Some(number))
              else this(x0, y0)
          )
        }
      }
    }

    for (x <- (0 until 9)) {
      val numbers = Set(1, 2, 3, 4, 5, 6, 7, 8, 9) --
        this.col(x).filter(_.isDefined).map(_.value.get) .toSet
      for (number <- numbers) {
        val positions =
          (for (y <- (0 until 9)) yield (x, y)).
          filter(t => this.candidates(t._1, t._2).contains(number)).toSet
          if (positions.size == 1) {
            val position = positions.head
            return this.map(
              (x0: Int, y0: Int) =>
                if (x0 == position._1 && y0 == position._2) SudokuCell(Some(number))
                else this(x0, y0)
            )
          }
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
