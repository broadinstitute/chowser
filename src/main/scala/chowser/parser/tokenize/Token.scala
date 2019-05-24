package chowser.parser.tokenize

trait Token {
  def string:String
  def pos: Int
  def size: Int
}

object Token {

}