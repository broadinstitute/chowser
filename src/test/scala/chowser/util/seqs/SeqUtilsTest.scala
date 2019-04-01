package chowser.util.seqs

import org.scalatest.FunSuite

class SeqUtilsTest extends FunSuite {
  test("findFromWhatIndex") {
    assert(SeqUtils.findFromWhatIndex(Seq(1))(_ > 0) == 0)
    assert(SeqUtils.findFromWhatIndex(Seq(1))(_ > 1) == 1)
    assert(SeqUtils.findFromWhatIndex(Seq(1, 2))(_ > 0) == 0)
    assert(SeqUtils.findFromWhatIndex(Seq(1, 2))(_ > 1) == 1)
    assert(SeqUtils.findFromWhatIndex(Seq(1, 2))(_ > 2) == 2)
    assert(SeqUtils.findFromWhatIndex(0 to 100)(_ * 3 > 125) == 42)
  }
}
