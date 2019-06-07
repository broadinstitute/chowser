package chowser.expressions.defs

import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.values.Value
import chowser.expressions.Result

trait Def[S <: Sig, R <: Ref[S]] {
  def sig: S = ref.sig

  def ref: R
}

object Def {

  sealed trait ScalarDef extends Def[ScalarSig, ScalarRef] {
    def ref: ScalarRef

    def result: Result
  }

  case class ValDef(ref: ScalarRef, result: Result) extends ScalarDef

  case class VarDef(ref: ScalarRef, result: Result) extends ScalarDef

  sealed trait OpDef[S <: Sig, R <: Ref[S], F] extends Def[S, R] {
    def function: F
  }

  case class UnitaryOpDef(ref: UnitaryOpRef, function: Value => Result)
    extends OpDef[UnitaryOpSig, UnitaryOpRef, Value => Result]

  case class BinaryOpDef(ref: BinaryOpRef, function: (Value, Value) => Result)
    extends OpDef[BinaryOpSig, BinaryOpRef, (Value, Value) => Result]

  case class FunctionDef(ref: FunctionRef, function: Seq[Value] => Result)
    extends OpDef[FunctionSig, FunctionRef, Seq[Value] => Result]

}