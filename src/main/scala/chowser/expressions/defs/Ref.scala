package chowser.expressions.defs

import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.Type

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
  case class FunctionRef(sig: FunctionSig, returnType: Type) extends Ref[FunctionSig]
}