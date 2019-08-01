package chowser.expressions.values

import chowser.expressions.defs.Def.{BinaryOpDef, FunctionDef}
import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig}
import chowser.expressions.tsv.TsvValues.TsvReaderObject
import chowser.expressions.{BoolType, FloatType, Identifier, IntType, LambdaType, ObjectType, StringType, Type}

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

  def floatCompare(opString: String, fun: (Double, Double) => Boolean): BinaryOpDef = {
    val ref = BinaryOpRef(BinaryOpSig(Identifier(None, opString), FloatType, FloatType), BoolType)
    val valueFunction =
      (lhs: Value, rhs: Value) => {
        lhs match {
          case FloatValue(lhsFloat) =>
            rhs match {
              case FloatValue(rhsFloat) => Right(BoolValue(fun(rhsFloat, lhsFloat)))
              case _ => Left(s"Expected Float value, but got ${rhs.asStringWithType}.")
            }
          case _ => Left(s"Expected Float value, but got ${lhs.asStringWithType}.")
        }
      }
    BinaryOpDef(ref, valueFunction)
  }

  def stringFunction1[R <: Value](id: Identifier, outType: Type, fun: String => R): FunctionDef = {
    val sig = FunctionSig(id, Seq(StringType))
    val ref = FunctionRef(sig, outType)
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

  def tsvReaderFunction1[R <: Value](id: Identifier, outType: Type, fun: TsvReaderObject => R): FunctionDef = {
    val sig = FunctionSig(id, Seq(ChowserObjectPool.tsvReader))
    val ref = FunctionRef(sig, outType)
    val valueFunction =
      (args: Seq[Value]) => {
        if(args.size > 1) {
          Left("Too many arguments, expected 1")
        } else if(args.isEmpty) {
          Left("Need one argument, got none.")
        } else {
          val arg = args.head
          arg match {
            case tsvReader: TsvReaderObject => Right(fun(tsvReader))
            case _ => Left(s"Expected TsvReader value, but got ${arg.asStringWithType}.")
          }
        }
      }
    FunctionDef(ref, valueFunction)
  }

  def tsvReaderColFilterFunction(id: Identifier,
                                 fun: (TsvReaderObject, String, LambdaValue) => TsvReaderObject): FunctionDef = {
    val sig = FunctionSig(id, Seq(ChowserObjectPool.tsvReader, StringType, LambdaType(1)))
    val ref = FunctionRef(sig, ChowserObjectPool.tsvReader)
    val valueFunction =
      (args: Seq[Value]) =>
        if(args.size != 3) {
          Left(s"Expected 3 arguments, got ${args.size}.")
        } else {
          args(0) match {
            case tsvReaderObject: TsvReaderObject =>
              args(1) match {
                case StringValue(colName) =>
                  args(2) match {
                    case lambdaValue: LambdaValue =>
                      Right(fun(tsvReaderObject, colName, lambdaValue))
                    case _ => Left(s"Expected function, but got ${args(2).asStringWithType}.")
                  }
                case _ => Left(s"Expected String, but got ${args(1).asStringWithType}.")
              }
            case _ => Left(s"Expected TsvReader, but got ${args(0).asStringWithType}.")
          }
        }
    FunctionDef(ref, valueFunction)
  }

}
