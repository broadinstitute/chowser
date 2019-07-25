package chowser.expressions.values

import chowser.expressions.defs.Def.BinaryOpDef
import chowser.expressions.defs.Ref.BinaryOpRef
import chowser.expressions.defs.Sig.BinaryOpSig
import chowser.expressions.{Failure, FloatType, Identifier, IntType, Result, Success}

object ValueOps {


  def intBinary(opString: String, fun: (Long, Long) => Long): BinaryOpDef = {
    val ref = BinaryOpRef(BinaryOpSig(Identifier(None, opString), IntType, IntType), IntType)
    val valueFunction =
      (lhs: Value, rhs: Value) => {
        lhs match {
          case IntValue(lhsInt) =>
            rhs match {
              case IntValue(rhsInt) => Success(IntValue(fun(rhsInt, lhsInt)))
              case _ => Failure(s"Expected Int value, but got ${rhs.asStringWithType}.")
            }
          case _ => Failure(s"Expected Int value, but got ${lhs.asStringWithType}.")
        }
      }
    BinaryOpDef(ref, valueFunction)
  }

  def floatBinary(opString: String, fun: (Double, Double) => Double): BinaryOpDef = {
    val ref = BinaryOpRef(BinaryOpSig(Identifier(None, opString), FloatType, FloatType), FloatType)
    val valueFunction =
      (lhs: Value, rhs: Value) => {
        lhs match {
          case FloatValue(lhsFloat) =>
            rhs match {
              case FloatValue(rhsFloat) => Success(FloatValue(fun(rhsFloat, lhsFloat)))
              case _ => Failure(s"Expected Float value, but got ${rhs.asStringWithType}.")
            }
          case _ => Failure(s"Expected Float value, but got ${lhs.asStringWithType}.")
        }
      }
    BinaryOpDef(ref, valueFunction)
  }


}
