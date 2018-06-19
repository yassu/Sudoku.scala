package sudoku

import scala.collection.JavaConversions._
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}

object Main {
  def checkEquivalenceMain(args: Array[String]) = {
    println("checkEquivalenceMain")

    val filename = args(0)
    val file = Paths.get(filename)
    val boards = Files.readAllLines(file, Charset.defaultCharset()).toList
         .filter(_ != "")
         .map(Board.parse(_).get)

    println("Loading is finished.")
    boards.groupBy(_.countMap.values.toList.sorted).foreach(t =>
    {
      println(t._1)
      t._2.foreach(println)
    })
  }

  def main(args: Array[String]) {
    checkEquivalenceMain(args)
  }
}
