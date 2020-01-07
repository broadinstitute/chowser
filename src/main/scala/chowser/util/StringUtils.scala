package chowser.util

object StringUtils {

  val escapes: Map[Char, String] = Map(
    '\b' -> "\\b",
    '\n' -> "\\n",
    '\t' -> "\\t",
    '\r' -> "\\r",
    '\f' -> "\\\f",
    '"' -> "\\\"",
    '\\' -> "\\\\",
  )

  def escape(string: String): String = string.flatMap(char => escapes.getOrElse(char, new String(Array(char))))

//  Backspace is replaced with \b.
//  Newline is replaced with \n.
//  Tab is replaced with \t.
//  Carriage return is replaced with \r.
//  Form feed is replaced with \f.
//  Double quote is replaced with \"
//  Backslash is replaced with \\
}
