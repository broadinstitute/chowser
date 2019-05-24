package chowser.parser.tokenize

case class ScanState(tokens: Seq[Token], pos: Int, string: String, remainder: String) {

  def addToken(token: Token): ScanState =
    ScanState(tokens :+ token, pos + token.size, string, string.substring(token.size))

}

object ScanState {
  def apply(string: String): ScanState = ScanState(Seq.empty, 0, string, string)
}
