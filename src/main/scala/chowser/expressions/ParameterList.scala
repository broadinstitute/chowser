package chowser.expressions

import chowser.expressions.Expression.ParamExpression.ParamExpressionId
import chowser.expressions.Expression.{ParamExpression, ParamId}

case class ParameterList(paramIds: Seq[ParamId], paramIdByExpression: Map[ParamExpressionId, ParamId]) {

  def withArgIdFor(argExpression: ParamExpression): ParameterList = {
    val argExpressionId = argExpression.id
    paramIdByExpression.get(argExpressionId) match {
      case Some(_) => this
      case None =>
        val (argIdsNew, argIdNew) = argExpression.posOpt match {
          case None => appendNewArgId(paramIds)
          case Some(pos) => getArgIdAtPos(paramIds, pos)
        }
        val argIdByExpressionNew = paramIdByExpression + (argExpressionId -> argIdNew)
        ParameterList(argIdsNew, argIdByExpressionNew)
    }
  }

  private def appendNewArgId(argIds: Seq[ParamId]): (Seq[ParamId], ParamId) = {
    val id = ParamId.createNew
    (argIds :+ id, id)
  }

  private def getArgIdAtPos(argIds: Seq[ParamId], pos: Int): (Seq[ParamId], ParamId) = {
    var argIdsNew: Seq[ParamId] = argIds
     while (argIdsNew.size < pos + 1) {
      argIdsNew = appendNewArgId(argIdsNew)._1
    }
    (argIdsNew, argIdsNew(pos))
  }

}

object ParameterList {
  def empty: ParameterList = ParameterList(Seq.empty, Map.empty)
}
