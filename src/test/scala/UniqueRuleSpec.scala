package sudoku

import org.scalatest.FunSpec

class OneColumnRuleSpec extends FunSpec {
  it ("uniquePositions") {
    val rule = new OneColumnUniqueRule(2, 9)
    assert(rule.uniquePositions == Seq(
      (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8)
    ))
  }
}

class OneRowRuleSpec extends FunSpec {
  it ("uniquePositions") {
    val rule = new OneRowUniqueRule(2, 9)
    assert(rule.uniquePositions == Seq(
      (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2)
    ))
  }
}

class OneCellRuleSpec extends FunSpec {
  it ("uniquePositions") {
    val rule = new OneCellUniqueRule(1, 2, 3)
    assert(rule.uniquePositions == Seq(
      (3, 6), (4, 6), (5, 6),
      (3, 7), (4, 7), (5, 7),
      (3, 8), (4, 8), (5, 8)
    ))
  }
}

class DiagonalLineRuleSpec extends FunSpec {
  it ("uniquePositions") {
    val rule = new DiagonalUniqueRule(9)
    assert(rule.uniquePositions == Seq(
      (0, 0),
      (1, 1),
      (2, 2),
      (3, 3),
      (4, 4),
      (5, 5),
      (6, 6),
      (7, 7),
      (8, 8)
    ))
  }
}

class InvDiagonalLineRuleSpec extends FunSpec {
  it ("uniquePositions") {
    val rule = new InvDiagonalUniqueRule(9)
    assert(rule.uniquePositions == Seq(
      (8, 0),
      (7, 1),
      (6, 2),
      (5, 3),
      (4, 4),
      (3, 5),
      (2, 6),
      (1, 7),
      (0, 8),
    ))
  }
}
