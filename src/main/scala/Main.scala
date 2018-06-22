package sudoku

import scala.collection.JavaConversions._
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}

object Main {
  def checkEquivalenceByNumberSetMain(args: Array[String]) = {
    println("checkEquivalenceByNumberSetMain")

    val file = Paths.get("data.txt")
    val boards = Files.readAllLines(file, Charset.defaultCharset()).toList
         .filter(_ != "")
         .map(s => {
           val board = Board.parse(s).get
          (board.toString, board.countMap.values.toList.sorted.toString)
         })

    println("Loading is finished.")

    boards.groupBy(t => t._2).foreach(t =>
    {
      println(t._1)
      println("count: " + t._2.size)
      t._2.foreach(tk => println(tk._1))
    })
  }

  def checkEquivalenceMain(args: Array[String]) = {
    println("checkEquivalenceMain")

    val file = Paths.get("data.txt")
    val boardFeatures = Files.readAllLines(file, Charset.defaultCharset()).toList
     .filter(_ != "")
     .map(s => {
       val board = SudokuXBoard.parse(s).get
       (board.toString, board.representative.toString)
     })
    println("Loading is finished.")

    println("Feature computation is finished.")

    val groups = boardFeatures.groupBy(_._2)
    groups.foreach(group => {
      println("=" * 90)
      println("feature: " + group._1)
      println("size: " + group._2.size)
      if (group._2.size > 1) {
        println("Problem is occured")
      }
      group._2.foreach(b => println(b._1))
    })
  }

  def main(args: Array[String]) {
    checkEquivalenceMain(args)
  }
}
