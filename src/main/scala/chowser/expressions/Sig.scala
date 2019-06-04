package chowser.expressions

sealed trait Sig {
  def id: Identifier
}

object Sig {

  case class ScalarSig(id: Identifier) extends Sig

  case class UnitaryOpSig(id: Identifier, argType: Type) extends Sig

  case class BinaryOpSig(id: Identifier, lhsType: Type, rhsType: Type) extends Sig

  case class MethodSig(id: Identifier, ownerType: Type, argTypes: Seq[Type]) extends Sig

}
