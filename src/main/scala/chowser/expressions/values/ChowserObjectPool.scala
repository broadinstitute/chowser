package chowser.expressions.values

import chowser.expressions.{Identifier, ObjectType}

object ChowserObjectPool {

  var idCounter: Long = 0

  private def createNewId(): ObjectValue.Id = {
    val id = ObjectValue.Id(idCounter)
    idCounter += 1
    id
  }

  private var objects: Map[ObjectValue.Id, ObjectValue] = Map.empty

  def createType(identifier: Identifier): ObjectType = {
    createInstance(ObjectType(_)(identifier))
  }

  def createType(name: String): ObjectType = createType(Identifier(None, name))

  val tsvReader: ObjectType = createType("TsvReader")

  def createInstance[T <: ObjectValue](maker: ObjectValue.Id => T): T = {
    val id = createNewId()
    val objectValue = maker(id)
    objects += (id -> objectValue)
    objectValue
  }

  def get(id: ObjectValue.Id): ObjectValue = objects(id)
}
