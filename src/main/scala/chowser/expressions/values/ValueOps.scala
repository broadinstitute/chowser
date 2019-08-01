package chowser.expressions.values

import chowser.expressions.defs.Def.{BinaryOpDef, FunctionDef}
import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig}
import chowser.expressions.{FloatType, Identifier, IntType, ObjectType, StringType}

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

  def stringFunction1[T <: ObjectValue](id: Identifier, fun: (String) => T): FunctionDef = {
    val sig = FunctionSig(id, Seq(StringType))
    val ref = FunctionRef(sig, ChowserObjectPool.tsvReader)
    val valueFunction =
      (args: Seq[Value]) => {
        if(args.size > 1) {
          Left("Too many arguments, expected 1")
        } else if(args.isEmpty) {
          Left("Need one argument, got none.")
        } else {
          val arg = args.head
          arg match {
            case StringValue(string) => Right(fun(string))
            case _ => Left(s"Expected String value, but got ${arg.asStringWithType}.")
          }
        }
      }
    FunctionDef(ref, valueFunction)
  }

}
