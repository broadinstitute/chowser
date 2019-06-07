package chowser.expressions.defs

import chowser.expressions.{Identifier, Type}

sealed trait Sig {
  def id: Identifier
}

object Sig {

  case class ScalarSig(id: Identifier) extends Sig

  case class UnitaryOpSig(id: Identifier, argType: Type) extends Sig

  case class BinaryOpSig(id: Identifier, lhsType: Type, rhsType: Type) extends Sig

  case class FunctionSig(id: Identifier, argTypes: Seq[Type]) extends Sig

}
