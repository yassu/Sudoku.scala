package sudoku

import scala.collection.mutable

object FuncUtil {
  def funcProduct[A](fs: (A => A, Int)): Seq[A => A] =
    if (fs._2 == 0) {
      val f: A => A = x => x
      Seq(f)
    }
    else {
      val beforeFuncs = funcProduct(fs._1, fs._2 - 1)
      funcProduct((fs._1, fs._2 - 1)) ++ Seq(
        (x: A) => fs._1(beforeFuncs.last(x))
      )
    }

  def funcProducts[A](fs: Seq[(A => A, Int)]): Set[A => A] =
    if (fs.isEmpty) Set()
    else {
      var funcs = mutable.Set[A => A]()
      var beforeFuncs = funcProducts(fs.drop(1))
      var nowFuncs = funcProduct(fs.head)

      funcs ++= beforeFuncs
      funcs ++= nowFuncs
      for (fs <- beforeFuncs; gs <- nowFuncs) {
        funcs += ((x:A)  => gs(fs(x)))
      }
      funcs.toSet
    }
}

