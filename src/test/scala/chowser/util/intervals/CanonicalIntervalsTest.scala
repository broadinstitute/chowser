package chowser.util.intervals

import org.scalatest.FunSuite

class CanonicalIntervalsTest extends FunSuite {
  test("adding regions") {
    var regions = CanonicalIntervals.empty
    regions :+= Interval(3, 5)
    assert(regions == CanonicalIntervals(Seq(Interval(3, 5))))
    regions :+= Interval(7, 10)
    assert(regions == CanonicalIntervals(Seq(Interval(3, 5), Interval(7, 10))))
    regions :+= Interval(2, 3)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 5), Interval(7, 10))))
    regions :+= Interval(12, 13)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 5), Interval(7, 10), Interval(12, 13))))
    regions :+= Interval(15, 17)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 5), Interval(7, 10), Interval(12, 13), Interval(15, 17))))
    regions :+= Interval(4, 8)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 13), Interval(15, 17))))
    regions :+= Interval(13, 20)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 20))))
    regions :+= Interval(19, 23)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 23))))
    regions :+= Interval(33, 42)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 23), Interval(33, 42))))
    regions :+= Interval(27, 29)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 23), Interval(27, 29), Interval(33, 42))))
    regions :+= Interval(27, 29)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 23), Interval(27, 29), Interval(33, 42))))
    regions :+= Interval(22, 28)
    assert(regions == CanonicalIntervals(Seq(Interval(2, 10), Interval(12, 29), Interval(33, 42))))
    regions :+= Interval(1, 50)
    assert(regions == CanonicalIntervals(Seq(Interval(1, 50))))
  }

}
