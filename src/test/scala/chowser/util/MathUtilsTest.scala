package chowser.util

import org.scalatest.FunSuite

class MathUtilsTest extends FunSuite {

  val tolerance = 1e-15

  def assertPrecise(result: Double, target: Double): Unit = {
    if (result != target) {
      val relError = Math.abs(result - target) / (0.5 * (Math.abs(result) + Math.abs(target)))
      assert(relError < tolerance,
        s"- result $result too different from target $target.")
    }
  }

  test("probit") {
    val delta = 1e-15
    assert(MathUtils.probit(-delta).isNaN)
    assert(MathUtils.probit(0.0).isNaN)
    assert(!MathUtils.probit(delta).isNaN)
    assertPrecise(MathUtils.probit(0.025), -1.9599639845400538)
    assertPrecise(MathUtils.probit(0.5), 0.0)
    assertPrecise(MathUtils.probit(0.975), 1.9599639845400538)
    assert(!MathUtils.probit(1.0 - delta).isNaN)
    assert(MathUtils.probit(1.0).isNaN)
    assert(MathUtils.probit(1.0 + delta).isNaN)

  }

}
