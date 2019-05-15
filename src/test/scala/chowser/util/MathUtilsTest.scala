package chowser.util

import org.scalatest.FunSuite

class MathUtilsTest extends FunSuite {

  val tolerance = 1e-15

  def assertPrecise(result: Double, target: Double): Unit = {
    val relError = Math.abs(result - target) / (0.5 * (Math.abs(result) + Math.abs(target)))
    assert(relError < tolerance,
      s"- result $result too different from target $target.")
  }

  test("probit") {
    assertPrecise(MathUtils.probit(0.025), -1.959964)

  }

}
