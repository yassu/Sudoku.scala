package sudoku.sudokux

import sudoku.{SudokuCell, Board, CommonSudokuBoard, UniqueLineRules}
import sudoku.{FuncUtil}
import scala.collection.mutable

class SudokuXBoard(cells: Seq[Seq[SudokuCell]]) extends CommonSudokuBoard(cells) {
  val rules = UniqueLineRules.sudokuXRules

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
              if (! map.keys.toSeq.contains(n)) {
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

  def ensure: Boolean =
    UniqueLineRules.sudokuXRules.forall(
      rule => {
        val numbers = rule.uniquePositions.map(t => this(t._1, t._2)).filter(_.isDefined)
        numbers == numbers.distinct
      }
    )

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
          return changeBoard(position._1, position._2, SudokuCell(Some(number)))
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
            return changeBoard(position._1, position._2, SudokuCell(Some(number)))
          }
      }
    }

    for (yC <- (0 until 3)) {
      for (xC <- (0 until 3)) {
        val numbers = (1 to 9).toSet --
          (for (y <- (3 * yC until 3 * yC + 3); x <- (3 * xC until 3 * xC + 3)) yield this(x, y))
          .filter(_.isDefined).map(_.value.get).toSet
        for (number <- numbers) {
          val positions =
            (for (y <- (3 * xC until 3 * xC + 3); x <- (3 * yC until 3 * yC + 3)) yield (x, y))
            .filter(t => this.candidates(t._1, t._2).contains(number)).toSet
          if (positions.size == 1) {
            val position = positions.head
            return changeBoard(position._1, position._2, SudokuCell(Some(number)))
          }
        }
      }
    }

    // TODO: Add test
    val diagNumbers = (0 until 9).toSet --
      (for (j <- (0 until 9)) yield this(j, j)).filter(_.isDefined).map(_.value.get).toSet
    for (number <- diagNumbers) {
      val positions = (for (j <- (0 until 9)) yield (j, j))
        .filter(t => this.candidates(t._1, t._2).contains(number)).toSet
      if (positions.size == 1) {
        val position = positions.head
        return changeBoard(position._1, position._2, SudokuCell(Some(number)))
      }
    }

    // TODO: Add test
    val invDiagNumers = (0 until 9).toSet --
      (for (j <- (0 until 9)) yield this(j, 8 - j)).filter(_.isDefined).map(_.value.get).toSet
    for (number <- invDiagNumers) {
      val positions = (for (j <- (0 until 9)) yield (j, 8 - j))
        .filter(t => this.candidates(t._1, t._2).contains(number)).toSet
      if (positions.size == 1) {
        val position = positions.head
        return changeBoard(position._1, position._2, SudokuCell(Some(number)))
      }
    }

    this
  }

  def solveNext: SudokuXBoard = {
    val board1 = this.solveNext1
    if (this != board1) {
      return board1.toSudokuXBoard
    }

    val board2 = this.solveNext2
    if (this != board2) {
      return board2.toSudokuXBoard
    }

    val board3 = this.solveNext3
    if (this != board3) {
      return board3
    }

    return this
  }

  def solve: Set[SudokuXBoard] = {
    def _solve(board: SudokuXBoard): SudokuXBoard = {
      val sol = board.solveNext
      if (board == sol) {
        board
      }
      else {
        _solve(sol)
      }
    }

    val sol = _solve(this)
    val ok = this.ensure

    if (! ok)
      Set()
    else if (sol.count == 81)
      Set(sol)
    else {
      val position = (for (y <- (0 until 9); x <- (0 until 9)) yield (x, y))
        .filter(t => ! this(t._1, t._2).isDefined)
        .minBy(t => this.candidates(t._1, t._2).size)
      val candidates = this.candidates(position._1, position._2)
      candidates
        .map(n => this.map(
          (x: Int, y: Int) =>
            if (x == position._1 && y == position._2) SudokuCell(Some(n))
            else this(x, y)
        ))
        .map(_.solve).flatMap {x => x}
        .filter(_.ensure).toSet
    }
  }

  override def changeBoard(x: Int, y: Int, cell: SudokuCell): SudokuXBoard =
    super.changeBoard(x, y, cell).toSudokuXBoard

  override def map(f: (Int, Int) => SudokuCell): SudokuXBoard = SudokuXBoard(
    (
      for (y <- (0 until 9)) yield
      (
        for (x <- (0 until 9)) yield f(x, y)
      ).toSeq
    ).toSeq
  )

  def count: Int = (for (y <- (0 until 9); x <- (0 until 9)) yield this(x, y)).count(_.isDefined)
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

    val fs = FuncUtil.funcProducts(Seq(
      ((b: SudokuXBoard) => b.centralReplacement, 1),
      ((b: SudokuXBoard) => b.rotate, 3),
      ((b: SudokuXBoard) => b.flip, 1),
    )).toSet

    for (f <- fs; g <- s3) yield f.compose((b: SudokuXBoard) => b.edgeReplacement(g)) andThen
      (b => b.normalize)
  }

  def apply(cells: Seq[Seq[SudokuCell]]): SudokuXBoard =
    new SudokuXBoard(cells)

  def parse(s: String, size: Int): Option[SudokuXBoard] = Board.parse(s, size) match {
    case Some(board) => Some(board.toSudokuXBoard)
    case None => None
  }

  implicit val ordering: Ordering[SudokuXBoard] = Ordering.by(b => b.toString)
}
