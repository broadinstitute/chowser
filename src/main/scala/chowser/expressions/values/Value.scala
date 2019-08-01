package chowser.expressions.values

import chowser.expressions.Expression._
import chowser.expressions._
import chowser.util.NumberParser.LongParser
import chowser.util.StringUtils

trait Value {
  def tpe: Type

  def asString: String

  def asStringWithType: String = asString + ": " + tpe.asString

  def asExpression: Expression

  def isLambdaValue: Boolean
}

object UnitValue extends Value {
  override def tpe: Type = UnitType

  override def asString: String = "()"

  override def asExpression: UnitLiteral.type = UnitLiteral

  override def isLambdaValue: Boolean = false
}

case class IntValue(value: Long) extends Value {
  override def tpe: IntType.type = IntType

  override def asString: String = value.toString

  override def asExpression: IntLiteral = IntLiteral(value)

  override def isLambdaValue: Boolean = false
}

case class FloatValue(value: Double) extends Value {
  override def tpe: FloatType.type = FloatType

  override def asString: String = {
    val defaultString = value.toString
    if (LongParser.isValid(defaultString)) {
      defaultString + ".0"
    } else {
      defaultString
    }
  }

  override def asExpression: FloatLiteral = FloatLiteral(value)

  override def isLambdaValue: Boolean = false
}

case class StringValue(value: String) extends Value {
  override def tpe: StringType.type = StringType

  override def asString: String =
    "\"" + StringUtils.escape(value) + "\""

  override def asExpression: StringLiteral = StringLiteral(value)

  override def isLambdaValue: Boolean = false
}

case class TupleValue(values: Seq[Value]) extends Value {
  override def tpe: Type = TupleType(values.map(_.tpe))

  override def asString: String = values.map(_.asString).mkString("(", ", ", ")")

  override def asExpression: TupleExpression = TupleExpression(values.map(_.asExpression))

  override def isLambdaValue: Boolean = false
}

case class LambdaValue(expression: Expression) extends Value {
  val argumentList: ArgumentList = expression.createArgumentList(ArgumentList.empty)

  def arity: Int = argumentList.argIds.size

  override def tpe: Type = LambdaType(arity)

  override def asString: String = expression.asString

  override def asExpression: Expression = expression

  override def isLambdaValue: Boolean = true
}

trait ObjectValue extends Value {
  def id: ObjectValue.Id

  override def asString: String = s"Object${id.index}"

  override def asExpression: Expression = ObjectLiteral(this)

  override def isLambdaValue: Boolean = false

  override def tpe: Type
}

trait ObjectInstanceValue extends ObjectValue {
  override def tpe: ObjectType
}

object ObjectValue {

  case class Id(index: Long)

}