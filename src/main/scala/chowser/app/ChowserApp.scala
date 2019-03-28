package chowser.app

object ChowserApp {

  def main(args: Array[String]): Unit = {
    val conf = new ChowserConf(args)
    println(conf.summary)
    println(conf.toChowserCommand)
    println("Done!")
  }

}
