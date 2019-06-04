package chowser.expressions.values

import chowser.expressions.{FloatType, IntType, StringType, Type, UnitType}

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

  override def asString: String = value.toString
}

case class StringValue(value: String) extends Value {
  override def tpe: StringType.type = StringType

  override def asString: String = "\"" + value.toString + "\""
}