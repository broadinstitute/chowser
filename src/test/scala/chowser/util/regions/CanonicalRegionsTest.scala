package chowser.util.regions

import org.scalatest.FunSuite

class CanonicalRegionsTest extends FunSuite {
  test("adding regions") {
    var regions = CanonicalRegions.empty
    regions :+= Region(3, 5)
    assert(regions == CanonicalRegions(Seq(Region(3, 5))))
    regions :+= Region(7, 10)
    assert(regions == CanonicalRegions(Seq(Region(3, 5), Region(7, 10))))
    regions :+= Region(2, 3)
    assert(regions == CanonicalRegions(Seq(Region(2, 5), Region(7, 10))))
    regions :+= Region(12, 13)
    assert(regions == CanonicalRegions(Seq(Region(2, 5), Region(7, 10), Region(12, 13))))
    regions :+= Region(15, 17)
    assert(regions == CanonicalRegions(Seq(Region(2, 5), Region(7, 10), Region(12, 13), Region(15, 17))))
    regions :+= Region(4, 8)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 13), Region(15, 17))))
    regions :+= Region(13, 20)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 20))))
    regions :+= Region(19, 23)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 23))))
    regions :+= Region(33, 42)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 23), Region(33, 42))))
    regions :+= Region(27, 29)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 23), Region(27, 29), Region(33, 42))))
    regions :+= Region(27, 29)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 23), Region(27, 29), Region(33, 42))))
    regions :+= Region(22, 28)
    assert(regions == CanonicalRegions(Seq(Region(2, 10), Region(12, 29), Region(33, 42))))
    regions :+= Region(1, 50)
    assert(regions == CanonicalRegions(Seq(Region(1, 50))))
  }

}
