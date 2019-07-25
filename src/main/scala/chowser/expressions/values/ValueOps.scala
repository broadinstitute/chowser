package chowser.expressions.values

import chowser.expressions.{Failure, Result, Success}

object ValueOps {


  def intBinary(fun: (Long, Long) => Long): (Value, Value) => Result =
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


}
