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

  def funcProducts(fs: List[(Int => Int, Int)]): List[Int => Int] =
    if (fs.isEmpty) List()
    else {
      val beforeFuncs = funcProducts(fs.drop(1))
      val nowFuncs = funcProduct(fs.head)
      funcProducts(fs.drop(1)) ++ funcProduct(fs.head) ++
        (for (fs <- beforeFuncs; gs <- nowFuncs) yield((x: Int) => gs(fs(x))))
    }
}

