package sudoku

import org.scalatest.FunSpec

class MathUtilSpec extends FunSpec {
  describe ("sqrtInt4") {
    assert(MathUtil.sqrtInt(4) == 2)
  }
  describe ("sqrtInt5") {
    assert(MathUtil.sqrtInt(5) == -1)
  }
  describe ("sqrtInt9") {
    assert(MathUtil.sqrtInt(9) == 3)
  }
}
