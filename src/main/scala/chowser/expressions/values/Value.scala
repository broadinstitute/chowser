package chowser.expressions.values

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