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

  def solve: Set[SudokuXBoard] =
    CommonSudokuBoard.solve(this, _.toSudokuXBoard)

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
