package chowser.expressions.tsv

import better.files.File
import chowser.expressions.{Expression, ObjectType, Type}
import chowser.expressions.values.{ChowserObjectPool, ObjectValue, Value}

object TsvValues {

  case class TsvReader(id: ObjectValue.Id)(val file: File) extends ObjectValue {
    override def tpe: Type = ChowserObjectPool.tsvReader
  }

  object TsvReader {
    def create(file: File): TsvReader = ChowserObjectPool.createInstance(TsvReader(_)(file))
  }

}
