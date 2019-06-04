package chowser.expressions

import chowser.expressions.Sig.{BinaryOpSig, MethodSig, ScalarSig, UnitaryOpSig}

trait Ref[S <: Sig] {
  def sig: S
  def returnType: Type
}

object Ref {
  sealed trait ScalarRef extends Ref[ScalarSig]
  case class ValRef(sig: ScalarSig, returnType: Type) extends ScalarRef
  case class VarRef(sig: ScalarSig, returnType: Type) extends ScalarRef
  case class UnitaryOpRef(sig: UnitaryOpSig, returnType: Type) extends Ref[UnitaryOpSig]
  case class BinaryOpRef(sig: BinaryOpSig, returnType: Type) extends Ref[BinaryOpSig]
  case class MethodRef(sig: MethodSig, returnType: Type) extends Ref[MethodSig]
}