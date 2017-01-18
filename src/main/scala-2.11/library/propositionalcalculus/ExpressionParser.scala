package library.propositionalcalculus

import org.parboiled2._

import scala.util.{Failure, Success}

/**
  * Created by amir on 04.04.16.
  */
class ExpressionParser(val input: ParserInput) extends Parser {

  def HeaderP = rule {
    Header ~ EOI
  }

  def Header: Rule1[(Seq[Expression], Expression)] = rule {
    zeroOrMore(Expression).separatedBy(",") ~ "|-" ~ Expression ~>
      ((seq: Seq[Expression], e: Expression) => { (seq, e) })
  }

  def ExpressionP = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[Expression] = rule {
    Disj ~ zeroOrMore("->" ~ Expression ~> Implication)
  }

  def Disj: Rule1[Expression] = rule {
    Conj ~ zeroOrMore("|" ~ Conj ~> Disjunction)
  }

  def Conj: Rule1[Expression] = rule {
    Neg ~ zeroOrMore("&" ~ Neg ~> Conjunction)
  }

  def Neg: Rule1[Expression] = rule {
    Var | ("!" ~ Neg ~> Negate) | ("(" ~ Expression ~ ")")
  }

  def Var: Rule1[PropVar] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.Digit)) ~> ((x: String) => PropVar(x))
  }
}

object ExpressionParser {
  def parseExpression(s: String): Option[Expression] = {
    val parser = new ExpressionParser(s)
    parser.ExpressionP.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }

  def parseHeader(s: String): Option[(Seq[Expression], Expression)] = {
    val parser = new ExpressionParser(s)
    parser.HeaderP.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }
}

