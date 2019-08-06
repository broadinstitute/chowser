package chowser.expressions

import chowser.expressions.Expression.ParamExpression.ParamExpressionId
import chowser.expressions.Expression.ParamId

case class ParamsToArgs(paramList: ParameterList, paramIdToArg: Map[ParamId, Expression]) {
  def getArgForParam(id: ParamExpressionId): Option[Expression] = {
    paramList.paramIdByExpression.get(id).flatMap(paramIdToArg.get)
  }
}

object ParamsToArgs {
  def empty: ParamsToArgs = ParamsToArgs(ParameterList.empty, Map.empty)

  def create(paramList: ParameterList, argList: Seq[Expression]): ParamsToArgs = {
    val paramIdToArg = paramList.paramIds.zip(argList).toMap
    ParamsToArgs(paramList, paramIdToArg)
  }
}
