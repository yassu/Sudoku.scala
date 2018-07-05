package sudoku

import scala.collection.JavaConversions._
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}
import sudoku.sudokux._

object Main {
  val filename = "su17ExtremeDiff500.txt"

  def solveSudokuMain(args: Array[String]) = {
    println("solveSudokuMain")

    val file = Paths.get(filename)
    val boards = Files.readAllLines(file, Charset.defaultCharset())
      .filter(_ != "")
      .map(s => {
        val board = SudokuBoard.parse(s, 9).get
        (board.toString, board.solve.map(_.toString))
      })
    boards.foreach(boards => {
      println(boards._1)
      boards._2.foreach(println)
      println("=" * 90)
    })
  }

  def checkEquivalenceByNumberSetMain(args: Array[String]) = {
    println("checkEquivalenceByNumberSetMain")

    val file = Paths.get(filename)
    val boards = Files.readAllLines(file, Charset.defaultCharset()).toList
         .filter(_ != "")
         .map(s => {
           val board = Board.parse(s, 9).get
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

    val file = Paths.get(filename)
    val fs = SudokuXBoard.equivalentTransformations
    val boardFeatures = Files.readAllLines(file, Charset.defaultCharset()).toList
     .filter(_ != "")
     .map(s => {
       val board = SudokuXBoard.parse(s, 9).get
       (board.toPrettyString, board.representative(fs).toPrettyString)
     })
    println("Loading is finished.")

    println("Feature computation is finished.")

    val groups = boardFeatures.groupBy(_._2)
    groups.foreach(group =>
      if (group._2.size > 1) {
        println("=" * 90)
        println("feature:")
        println(group._1.replace("\n", ""))
        println(group._1)
        println("size: " + group._2.size)
        println()

        group._2.foreach(b => println(b._1.replace("\n", "")))
        println()
        group._2.foreach(b => {
          println(b._1)
          println()
        })
      }
    )
  }

  def solveSudokuXMain(args: Array[String]) = {
    println("solveSudokuXMain")

    val file = Paths.get(filename)

    val boardInfos = Files.readAllLines(file, Charset.defaultCharset()).take(5)
      .map(s => {
        val board = SudokuXBoard.parse(s, 9).get
        (board.toString, board.solve.map(b => b.toString))
      })
    boardInfos.foreach(s => {
      println(s._1)
      println("size: " + s._2.size.toString)
      s._2.foreach(
        bs => println(" " * 4 + bs.toString)
      )
    })
  }

  def main(args: Array[String]) {
    solveSudokuMain(args)
  }
}
