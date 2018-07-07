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

  it ("digitString") {
    assert(MathUtil.digit(0) == 1)
    assert(MathUtil.digit(1) == 1)
    assert(MathUtil.digit(9) == 1)
    assert(MathUtil.digit(16) == 2)
    assert(MathUtil.digit(25) == 2)
    assert(MathUtil.digit(36) == 2)
    assert(MathUtil.digit(49) == 2)
    assert(MathUtil.digit(64) == 2)
    assert(MathUtil.digit(81) == 2)
    assert(MathUtil.digit(100) == 3)
  }
}
