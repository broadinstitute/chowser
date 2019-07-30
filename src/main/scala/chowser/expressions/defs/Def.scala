package chowser.expressions.defs

import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.values.Value

trait Def[S <: Sig, R <: Ref[S]] {
  def sig: S = ref.sig

  def ref: R
}

object Def {

  sealed trait ScalarDef extends Def[ScalarSig, ScalarRef] {
    def ref: ScalarRef

    def result: Either[String, Value]
  }

  case class ValDef(ref: ScalarRef, result: Either[String, Value]) extends ScalarDef

  case class VarDef(ref: ScalarRef, result: Either[String, Value]) extends ScalarDef

  sealed trait OpDef[S <: Sig, R <: Ref[S], F] extends Def[S, R] {
    def function: F
  }

  case class UnitaryOpDef(ref: UnitaryOpRef, function: Value => Either[String, Value])
    extends OpDef[UnitaryOpSig, UnitaryOpRef, Value => Either[String, Value]]

  case class BinaryOpDef(ref: BinaryOpRef, function: (Value, Value) => Either[String, Value])
    extends OpDef[BinaryOpSig, BinaryOpRef, (Value, Value) => Either[String, Value]]

  case class FunctionDef(ref: FunctionRef, function: Seq[Value] => Either[String, Value])
    extends OpDef[FunctionSig, FunctionRef, Seq[Value] => Either[String, Value]]

}