package sudoku

object FuncUtil {
  def funcProduct(fs: (Int => Int, Int)): List[Int => Int] =
    if (fs._2 == 0) {
      val f: Int => Int = x => x
      List(f)
    }
    else {
      val beforeFuncs = funcProduct(fs._1, fs._2 - 1)
      funcProduct((fs._1, fs._2 - 1)) ++ List(
        (x: Int) => fs._1(beforeFuncs.last(x))
      )
    }
}

