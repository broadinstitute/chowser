package chowser.util.io

case class ResourceConfig(keyFileOpt: Option[InputId]) {
  def withKeyFile(keyFile: InputId): ResourceConfig = copy(keyFileOpt = Some(keyFile))
}

object ResourceConfig {
  def empty: ResourceConfig = ResourceConfig(None)
}

