package library.formalarithmetic

import org.parboiled2._

import scala.util.{Failure, Success}

/**
  * @author amir.
  */
class FormalArithmeticParser(val input: ParserInput) extends Parser {

  def HeaderP = rule {
    Header ~ EOI
  }

  def Header: Rule1[(Seq[FAExpression], FAExpression)] = rule {
    zeroOrMore(FAExpression).separatedBy(",") ~ "|-" ~ FAExpression ~>
      ((seq: Seq[FAExpression], e: FAExpression) => {
        (seq, e)
      })
  }

  def ExpressionP = rule {
    FAExpression ~ EOI
  }

  def FAExpression: Rule1[FAExpression] = rule {
    Disj ~ zeroOrMore("->" ~ FAExpression ~> Implication)
  }

  def Disj: Rule1[FAExpression] = rule {
    Conj ~ zeroOrMore("|" ~ Conj ~> Disjunction)
  }

  def Conj: Rule1[FAExpression] = rule {
    Unary ~ zeroOrMore("&" ~ Unary ~> Conjunction)
  }

  def Unary: Rule1[FAExpression] = rule {
    PredicateP | ("!" ~ Neg ~> Negate) | ("(" ~ FAExpression ~ ")") |
      "@" ~ FuncVar ~ Unary ~> AnySubst | "?" ~ FuncVar ~ Unary ~> ExistsSubst
  }

  def PredicateP: Rule1[FAExpression] = rule {
    capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.Digit)) ~
      optional("(" ~ oneOrMore(TermP).separatedBy(",") ~ ")") ~>
      ((name: String, vars: Option[Seq[Term]]) => {
        Predicate(name, vars.getOrElse(Seq()))
      }) | Equal
  }

  def Equal: Rule1[FAExpression] = rule {
    TermP ~ "=" ~ TermP ~> EqualityPredicate
  }

  def Neg: Rule1[FAExpression] = rule {
    ("!" ~ Neg ~> Negate) | ("(" ~ FAExpression ~ ")")
  }


  //========= Terms ========

  def TermP: Rule1[Term] = rule {
    Summ ~ zeroOrMore("+" ~ Summ ~> Sum)
  }

  def Summ: Rule1[Term] = rule {
    Mult ~ zeroOrMore("*" ~ Mult ~> Multiplication)
  }

  def Mult: Rule1[Term] = rule {
    (Func | ("(" ~ TermP ~ ")") | ("0" ~ push(Zero()))) ~ zeroOrMore(capture("'")) ~>
      ((t: Term, s: Seq[String]) => {
        rec(s.length, t)
      })
  }

  def rec(i: Int, t: Term): Term = if (i == 0) t else rec(i - 1, PlusOne(t))

  def Func: Rule1[Term] = rule {
    FuncVar ~ optional("(" ~ oneOrMore(TermP).separatedBy(",") ~ ")") ~>
      ((name: String, vars: Option[Seq[Term]]) => {
        if (vars.isEmpty) Variable(name) else Function(name, vars.get())
      })
  }

  def FuncVar: Rule1[String] = rule {
    capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.Digit))
  }

}

object FormalArithmeticParser {
  def parseExpression(s: String): Option[FAExpression] = {
    val parser = new FormalArithmeticParser(s)
    println(s)
    parser.ExpressionP.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }

  def parseHeader(s: String): Option[(Seq[FAExpression], FAExpression)] = {
    val parser = new FormalArithmeticParser(s)
    println(s)
    parser.HeaderP.run() match {
      case Success(result) => Some(result)
      case Failure(e: ParseError) =>
        println("Expression is not valid: " + parser.formatError(e))
        None
      case _ => None
    }
  }
}

