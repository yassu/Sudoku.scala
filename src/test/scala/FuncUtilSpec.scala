package sudoku

import org.scalatest.FunSpec

class FuncUtilSpec extends FunSpec {
  describe ("funcProduct") {
    it ("empty argument") {
      val functions = FuncUtil.funcProduct((
      (x: Int) => 2 * x,
      3
      ))
      assert(functions.map(f => f(5)).toSet == Set(5, 5 * 2, 5 * 2 * 2, 5 * 2 * 2 * 2))
    }
  }
}
