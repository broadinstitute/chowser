package chowser.expressions.defs

import chowser.expressions.{Identifier, Type}

sealed trait Sig {
  def id: Identifier
  def asString: String
}

object Sig {

  case class ScalarSig(id: Identifier) extends Sig {
    override def asString: String = id.asString
  }

  case class UnitaryOpSig(id: Identifier, argType: Type) extends Sig {
    override def asString: String = s"${id.asString}(${argType.asString})"
  }

  case class BinaryOpSig(id: Identifier, lhsType: Type, rhsType: Type) extends Sig {
    override def asString: String = s"(${lhsType.asString})${id.asString}(${rhsType.asString})"
  }

  case class FunctionSig(id: Identifier, argTypes: Seq[Type]) extends Sig {
    override def asString: String = s"(${argTypes.map(_.asString).mkString(", ")})${id.asString}"
  }

}
