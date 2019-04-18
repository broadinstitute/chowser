package chowser.util.genomics

import chowser.util.NumberParser

sealed trait Chromosome {
  def inEnsembleNotation: String

  def inUcscNotation: String = "chr" + inEnsembleNotation

}

object Chromosome {

  case class Numbered(n: Int) extends Chromosome {
    override def inEnsembleNotation: String = n.toString
  }

  case class Lettered(letter: Char) extends Chromosome {
    override def inEnsembleNotation: String = letter.toString
  }

  def parse(string: String): Either[String, Chromosome] = {
    val trimmed = string.trim
    val cleaned = if (trimmed.startsWith("chr")) trimmed.substring(3) else trimmed
    NumberParser.UnsignedIntParser.parseOpt(cleaned) match {
      case Some(number) => Right(Numbered(number))
      case None =>
        def invalidChromosomeLeft(string: String): Left[String, Chromosome] = Left(s"Invalid chromosome $trimmed")

        if (cleaned.size != 1) {
          invalidChromosomeLeft(trimmed)
        } else {
          val char = cleaned.charAt(0)
          if (char.isLetter) {
            Right(Lettered(char))
          } else {
            invalidChromosomeLeft(trimmed)
          }
        }
    }

  }

  object Ordering extends scala.math.Ordering[Chromosome] {
    override def compare(chr1: Chromosome, chr2: Chromosome): Int = {
      (chr1, chr2) match {
        case (Numbered(n1), Numbered(n2)) => n1 - n2
        case (Numbered(_), Lettered(_)) => -1
        case (Lettered(_), Numbered(_)) => 1
        case (Lettered(char1), Lettered(char2)) => char1 - char2
      }
    }
  }
}
