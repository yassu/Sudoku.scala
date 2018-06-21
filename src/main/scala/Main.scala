package sudoku

import scala.collection.JavaConversions._
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}

object Main {
  def checkEquivalenceMain(args: Array[String]) = {
    println("checkEquivalenceMain")

    // val filename = args(0)
    val file = Paths.get("data.txt")
    val boards = Files.readAllLines(file, Charset.defaultCharset()).toList
         .filter(_ != "")
         .map(s => {
           val board = Board.parse(s).get
          (board.toString, board.countMap.values.toList.sorted.toString)
         })

def composite(f: Int => Int, g: Int => Int): Int => Int  = x => f(g(x))
    println("Loading is finished.")

    boards.groupBy(t => t._2).foreach(t =>
    {
      println(t._1)
      println("count: " + t._2.size)
      t._2.foreach(tk => println(tk._1))
    })
  }

  def main(args: Array[String]) {
    checkEquivalenceMain(args)
  }
}