package chowser.expressions

case class Identifier(namespaceOpt: Option[Identifier], localName: String) {
  def /(localName: String): Identifier = Identifier(Some(this), localName)
  def /(id: Identifier): Identifier = {
    id.namespaceOpt match {
      case Some(namespace) => Identifier(Some(/(namespace)), id.localName)
      case None => Identifier(Some(this), id.localName)
    }
  }

  def asString: String = {
    namespaceOpt match {
      case Some(namespace) => namespace.asString + "." + localName
      case None => localName
    }
  }
}

object Identifier {
  def /(localName: String): Identifier = Identifier(None, localName)
}

