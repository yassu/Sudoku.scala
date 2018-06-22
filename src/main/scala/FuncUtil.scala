package sudoku

object FuncUtil {
  def funcProduct[A](fs: (A => A, Int)): List[A => A] =
    if (fs._2 == 0) {
      val f: A => A = x => x
      List(f)
    }
    else {
      val beforeFuncs = funcProduct(fs._1, fs._2 - 1)
      funcProduct((fs._1, fs._2 - 1)) ++ List(
        (x: A) => fs._1(beforeFuncs.last(x))
      )
    }

  def funcProducts[A](fs: List[(A => A, Int)]): List[A => A] =
    if (fs.isEmpty) List()
    else {
      val beforeFuncs = funcProducts(fs.drop(1))
      val nowFuncs = funcProduct(fs.head)
      funcProducts(fs.drop(1)) ++ funcProduct(fs.head) ++
        (for (fs <- beforeFuncs; gs <- nowFuncs) yield((x: A) => gs(fs(x))))
    }
}

