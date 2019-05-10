package chowser.execute

import better.files.File

import scala.sys.process.Process

object ExecutionUtils {
  def runBashScript(script: String): Unit = {
    File.usingTemporaryFile() { scriptFile =>
      scriptFile.overwrite(script)
      Process(Seq("bash", scriptFile.toString())).!
    }
  }
}
