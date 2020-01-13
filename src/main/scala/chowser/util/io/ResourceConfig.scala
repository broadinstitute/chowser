package chowser.util.io

case class ResourceConfig(keyFileOpt: Option[InputId]) {
  def withKeyFile(keyFile: InputId): ResourceConfig = copy(keyFileOpt = Some(keyFile))
}

object ResourceConfig {
  def empty: ResourceConfig = ResourceConfig(None)

  def forKeyFile(keyFile: InputId): ResourceConfig = ResourceConfig(Some(keyFile))

  def forKeyFileOpt(keyFileOpt: Option[InputId]): ResourceConfig = keyFileOpt.fold(empty)(forKeyFile)
}

