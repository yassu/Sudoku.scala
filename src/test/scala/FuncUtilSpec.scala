package sudoku

import org.scalatest.FunSpec

class FuncUtilSpec extends FunSpec {
  describe ("funcProduct") {
    it ("only one argument") {
      val functions = FuncUtil.funcProduct((
      (x: Int) => 2 * x,
      3
      ))
      assert(functions.map(f => f(5)).toSet == Set(5, 5 * 2, 5 * 2 * 2, 5 * 2 * 2 * 2))
    }

    describe ("funcProducts") {
      it ("only two argument") {
        val functions = FuncUtil.funcProducts(List(
          ((x: Int) => 2 * x, 3),
          ((x: Int) => 3 * x, 2),
        ))
        assert(functions.map(f => f(5)).toSet == Set(
          5, 5 * 2, 5 * 2 * 2, 5 * 2 * 2 * 2,
          5 * 3, 5 * 2 * 3, 5 * 2 * 2 * 3, 5 * 2 * 2 * 2 * 3,
          5 * 3 * 3, 5 * 2 * 3 * 3, 5 * 2 * 2 * 3 * 3, 5 * 2 * 2 * 2 * 3 * 3,
        ))
      }
    }
  }
}
