package chowser.expressions

import chowser.expressions.Expression.ArgExpression.ArgExpressionId
import chowser.expressions.Expression.{ArgExpression, ArgId}

case class ArgumentList(argIds: Seq[ArgId], argIdByExpression: Map[ArgExpressionId, ArgId]) {

  def withArgIdFor(argExpression: ArgExpression): ArgumentList = {
    val argExpressionId = argExpression.id
    argIdByExpression.get(argExpressionId) match {
      case Some(_) => this
      case None =>
        val (argIdsNew, argIdNew) = argExpression.posOpt match {
          case None => appendNewArgId(argIds)
          case Some(pos) => getArgIdAtPos(argIds, pos)
        }
        val argIdByExpressionNew = argIdByExpression + (argExpressionId -> argIdNew)
        ArgumentList(argIdsNew, argIdByExpressionNew)
    }
  }

  private def appendNewArgId(argIds: Seq[ArgId]): (Seq[ArgId], ArgId) = {
    val id = ArgId.createNew
    (argIds :+ id, id)
  }

  private def getArgIdAtPos(argIds: Seq[ArgId], pos: Int): (Seq[ArgId], ArgId) = {
    var argIdsNew: Seq[ArgId] = argIds
     while (argIdsNew.size < pos + 1) {
      argIdsNew = appendNewArgId(argIdsNew)._1
    }
    (argIdsNew, argIdsNew(pos))
  }

}
