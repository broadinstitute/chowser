package chowser.expressions.values

import chowser.expressions.defs.Def.BinaryOpDef
import chowser.expressions.defs.Ref.BinaryOpRef
import chowser.expressions.defs.Sig.BinaryOpSig
import chowser.expressions.{FloatType, Identifier, IntType}

object ValueOps {


  def intBinary(opString: String, fun: (Long, Long) => Long): BinaryOpDef = {
    val ref = BinaryOpRef(BinaryOpSig(Identifier(None, opString), IntType, IntType), IntType)
    val valueFunction =
      (lhs: Value, rhs: Value) => {
        lhs match {
          case IntValue(lhsInt) =>
            rhs match {
              case IntValue(rhsInt) => Right(IntValue(fun(rhsInt, lhsInt)))
              case _ => Left(s"Expected Int value, but got ${rhs.asStringWithType}.")
            }
          case _ => Left(s"Expected Int value, but got ${lhs.asStringWithType}.")
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
              case FloatValue(rhsFloat) => Right(FloatValue(fun(rhsFloat, lhsFloat)))
              case _ => Left(s"Expected Float value, but got ${rhs.asStringWithType}.")
            }
          case _ => Left(s"Expected Float value, but got ${lhs.asStringWithType}.")
        }
      }
    BinaryOpDef(ref, valueFunction)
  }


}
