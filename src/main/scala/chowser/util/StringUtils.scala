package chowser.util

object StringUtils {

  val escapes: Map[Char, Seq[Char]] = Map(
    '\b' -> Seq('\\', 'b'),
    '\n' -> Seq('\\', 'n'),
    '\t' -> Seq('\\', 't'),
    '\r' -> Seq('\\', 'r'),
    '\f' -> Seq('\\', '\f'),
    '"' -> Seq('\\', '"'),
    '\\' -> Seq('\\', '\\'),
  )

  def escape(string: String): String = string.flatMap(char => escapes.getOrElse(char, Seq(char)))

//  Backspace is replaced with \b.
//  Newline is replaced with \n.
//  Tab is replaced with \t.
//  Carriage return is replaced with \r.
//  Form feed is replaced with \f.
//  Double quote is replaced with \"
//  Backslash is replaced with \\
}
