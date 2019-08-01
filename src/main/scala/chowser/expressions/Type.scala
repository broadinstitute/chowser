package chowser.expressions

import chowser.expressions.Expression.TypeLiteral
import chowser.expressions.values.{ObjectValue, Value}

sealed trait Type extends Value {
  override def tpe: TypeType.type = TypeType

  override def asExpression: Expression = TypeLiteral(this)

  override def isLambdaValue: Boolean = false

}

object TypeType extends Type {
  override def asString: String = "Type"
}

object IntType extends Type {
  override def asString: String = "Int"
}

object FloatType extends Type {
  override def asString: String = "Float"
}

object BoolType extends Type {
  override def asString: String = "Bool"
}

object StringType extends Type {
  override def asString: String = "String"
}

object UnitType extends Type {
  override def asString: String = "Unit"
}

case class TupleType(argTypes: Seq[Type]) extends Type {
  override def asString: String = argTypes.map(_.asString).mkString("(", ", ", ")")
}

case class LambdaType(arity: Int) extends Type {
  override def asString: String = s"L$arity"
}

case class ObjectType(id: ObjectValue.Id)(val name: Identifier)
  extends ObjectValue with Type {
  override def asString: String = name.asString
}

