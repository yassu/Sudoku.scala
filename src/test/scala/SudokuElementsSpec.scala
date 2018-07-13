package sudoku

import org.scalatest.FunSpec

class SudokuBoardSpec extends FunSpec {
    val exampleBoard = SudokuBoard(Seq(
      Seq(
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(9)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(7)), SudokuCell(None), SudokuCell(None),
        SudokuCell(Some(4)), SudokuCell(Some(8)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(5)), SudokuCell(Some(7)), SudokuCell(Some(8)),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(4)),
        SudokuCell(None), SudokuCell(None), SudokuCell(None),
      ),

      Seq(
        SudokuCell(None), SudokuCell(Some(8)), SudokuCell(None),
        SudokuCell(None), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(6)), SudokuCell(None), SudokuCell(Some(1)),
      ),
      Seq(
        SudokuCell(None), SudokuCell(Some(5)), SudokuCell(Some(2)),
        SudokuCell(Some(1)), SudokuCell(Some(7)), SudokuCell(None),
        SudokuCell(Some(9)), SudokuCell(Some(4)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(Some(4)), SudokuCell(None), SudokuCell(Some(3)),
        SudokuCell(Some(9)), SudokuCell(Some(2)), SudokuCell(None),
        SudokuCell(None), SudokuCell(Some(7)), SudokuCell(Some(8)),
      ),

      Seq(
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(1)),
        SudokuCell(Some(3)), SudokuCell(Some(6)), SudokuCell(None),
        SudokuCell(Some(2)), SudokuCell(Some(5)), SudokuCell(None),
      ),
      Seq(
        SudokuCell(None), SudokuCell(Some(4)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(None), SudokuCell(Some(2)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(6)),
      ),
      Seq(
        SudokuCell(Some(2)), SudokuCell(Some(3)), SudokuCell(Some(6)),
        SudokuCell(None), SudokuCell(Some(1)), SudokuCell(Some(5)),
        SudokuCell(Some(8)), SudokuCell(Some(9)), SudokuCell(Some(7)),
      ),
    ))

  it ("SudokuBoard should be initialized.") {
    SudokuBoard(Seq(
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
      Seq(SudokuCell(None), SudokuCell(None), SudokuCell(None), SudokuCell(None)),
    ))
  }

  it ("countSet") {
    assert(exampleBoard.countSet == Set(
      5, 5, 5, 5, 5, 5, 5, 7, 5,
    ))
  }

  it ("countMap") {
    assert(exampleBoard.countMap == Map(
      1 -> 5,
      2 -> 5,
      3 -> 5,
      4 -> 5,
      5 -> 5,
      6 -> 5,
      7 -> 5,
      8 -> 7,
      9 -> 5,
      ))
  }

  it ("easy problem") {
    val easyProblem = SudokuBoard.parse(
      "..8.5...." +
      "......9.6" +
      "12..7...." +
      "8....6.95" +
      "4.3....12" +
      "6153.94.7" +
      "289467.53" +
      "751.93.6." +
      "3.4.1827."
      ).get

    val ans = SudokuBoard.parse(
      "938652741" +
      "547831926" +
      "126974538" +
      "872146395" +
      "493785612" +
      "615329487" +
      "289467153" +
      "751293864" +
      "364518279"
   ).get
   val sols = easyProblem.solve
   assert(sols.size == 1)
   val sol = sols.head
   assert(sols == Set(ans))
  }

  it ("2x2 easy problem") {
    val easyProblem = SudokuBoard.parse(
      "1..2" +
      ".2.." +
      "..4." +
      "4..3"
    ).get
    val sols = easyProblem.solve
    assert(sols.size == 1)
    assert(sols.head.ensure)
  }

  it ("16x16 problem") {
    val board = SudokuBoard.parse(
      "0802....050613......12......14.." +
      "0515100414....16090107..12..0311" +
      "12070311....01....13..14....0208" +
      "14........07101202..08..01150905" +
      "0113120508..15..0406..07....1116" +
      "11........12..1008150916..01..02" +
      "......06..1609..........0410..13" +
      "..16..0307....0414..13..06..12.." +
      "061408....15....12..040211..1009" +
      "......1306..14..01....03........" +
      "..121510....110706..16....05...." +
      "..11..0209..............08..01.." +
      "04..0612....08..0709........13.." +
      "..09....1502......0406..14....10" +
      "02....15..05..................03" +
      "....07....13......11..081512...."
    ).get
    val resBoard = SudokuBoard.parse(
      "08020109050613031516121110041407" +
      "05151004140802160901070612130311" +
      "12070311040901151013051416060208" +
      "14061316110710120203080401150905" +
      "01131205080315020406100709141116" +
      "11041407131206100815091603010502" +
      "15080206011609141112030504100713" +
      "10160903071105041402130106081215" +
      "06140801031516131205040211071009" +
      "09050413061014080107110302161512" +
      "03121510020111070608160913050414" +
      "07111602090412051310141508030106" +
      "04030612161408110709151005021301" +
      "13090508150207010304061214111610" +
      "02101115120504061614011307090803" +
      "16010714101303090511020815120604"
    ).get
    val sols = board.solve
    assert(sols.size == 1)
    assert(sols.head == resBoard)
  }

  it ("hard problem") {
    val board = SudokuBoard.parse(
      "..53....." +
      "8......2." +
      ".7..1.5.." +
      "4....53.." +
      ".1..7...6" +
      "..32...8." +
      ".6.5....9" +
      "..4....3." +
      ".....97.."
    ).get
    val sols = board.solve
    val sol = sols.head
    assert(sols.size == 1 && sol.count == 81 && sol.ensure)
  }
}
