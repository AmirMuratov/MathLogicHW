package library.ordinals

import org.parboiled2.{CharPredicate, _}

import scala.util.{Failure, Success}

/**
  * @author amir.
  */
class OrdinalParser(val input: ParserInput) extends Parser {

  def OrdinalsP = rule {
    TwoOrdinals ~ EOI
  }

  def TwoOrdinals: Rule1[(Ordinal, Ordinal)] = rule {
    (Summ ~ "=" ~ Summ) ~> ((x: Ordinal, y: Ordinal) => {
      (x:Ordinal, y:Ordinal)
    })
  }

  def Summ: Rule1[Ordinal] = rule {
    Mult ~ zeroOrMore("+" ~ Mult ~> Sum)
  }

  def Mult: Rule1[Ordinal] = rule {
    Pow ~ zeroOrMore("*" ~ Pow ~> Multiplication)
  }

  def Pow: Rule1[Ordinal] = rule {
    Term ~ zeroOrMore("^" ~ Pow ~> Power)
  }

  def Term: Rule1[Ordinal] = rule {
    w | Numb | ("(" ~ Summ ~ ")")
  }

  def w: Rule1[W] = rule {
    "w" ~ push(W())
  }

  def Numb: Rule1[NatNumber] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => NatNumber(Integer.valueOf(x)))
  }
}

object OrdinalParser {
  def parse2Ordinals(s: String): Option[(Ordinal, Ordinal)] = {
    val parser = new OrdinalParser(s)
    parser.OrdinalsP.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }

}