package chowser.genomics

import chowser.util.NumberParser

sealed trait Chromosome extends Ordered[Chromosome] {
  override def toString: String = inEnsembleNotation

  def inEnsembleNotation: String

  def inUcscNotation: String = "chr" + inEnsembleNotation
}

object Chromosome {

  case class Numbered(n: Int) extends Chromosome {
    override def inEnsembleNotation: String = n.toString

    override def compare(that: Chromosome): Int = {
      that match {
        case Numbered(nThat) => n - nThat
        case _: Lettered => -1
      }
    }
  }

  case class Lettered(letter: Char) extends Chromosome {
    override def inEnsembleNotation: String = letter.toString

    override def compare(that: Chromosome): Int = {
      that match {
        case _: Numbered => 1
        case Lettered(letterThat) => letter - letterThat
      }
    }
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
}
