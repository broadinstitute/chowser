package chowser.util

import org.scalatest.FunSuite

class NumberParserTest extends FunSuite{
  test("DoubleParser") {
    println(NumberParser.DoubleParser.isValid("123.456e123 hello"))
  }

}
