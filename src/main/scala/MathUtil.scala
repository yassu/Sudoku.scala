package sudoku

object MathUtil {
  def sqrtInt(n: Int): Int = Map(
    0 -> 0,
    1 -> 1,
    4 -> 2,
    9 -> 3,
    16 -> 4,
    25 -> 5,
    36 -> 6,
    49 -> 7,
    64 -> 8,
    81 -> 9,
    100 -> 10,
  ).getOrElse(n, -1)
}
