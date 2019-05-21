package chowser.expressions.values

trait Type extends Value {
  override def tpe: TypeType.type = TypeType
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

object UnitType extends Type {
  override def asString: String = "Unit"
}
