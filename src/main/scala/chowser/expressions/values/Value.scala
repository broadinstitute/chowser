package chowser.expressions.values

import chowser.expressions.{Expression, FloatType, IntType, LambdaType, StringType, TupleType, Type, UnitType}
import chowser.util.NumberParser.LongParser
import chowser.util.StringUtils

trait Value {
  def tpe: Type
  def asString: String
  def asStringWithType: String = asString + ": " + tpe.asString
}

object UnitValue extends Value {
  override def tpe: Type = UnitType

  override def asString: String = "()"

}

case class IntValue(value: Long) extends Value {
  override def tpe: IntType.type = IntType

  override def asString: String = value.toString

}

case class FloatValue(value: Double) extends Value {
  override def tpe: FloatType.type = FloatType

  override def asString: String = {
    val defaultString = value.toString
    if(LongParser.isValid(defaultString)) {
      defaultString + ".0"
    } else {
      defaultString
    }
  }
}

case class StringValue(value: String) extends Value {
  override def tpe: StringType.type = StringType

  override def asString: String =
    "\"" + StringUtils.escape(value) + "\""
}

case class TupleValue(values: Seq[Value]) extends Value {
  override def tpe: Type = TupleType(values.map(_.tpe))

  override def asString: String = values.map(_.asString).mkString("(", ", ", ")")
}

case class LambdaValue(expression: Expression, arity: Int) extends Value {
  override def tpe: Type = LambdaType(arity)

  override def asString: String = expression.asString
}