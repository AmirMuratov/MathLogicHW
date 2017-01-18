package library.ordinals

/**
  * @author amir.
  */

sealed trait Ordinal {

  def toCNF: CNF_T = this match {
    case W() => CNF(CNF(Atom(1), Atom(1)), Atom(0))
    case NatNumber(n) => Atom(n)
    case Sum(a, b) => a.toCNF + b.toCNF
    case Multiplication(a, b) => a.toCNF * b.toCNF
    case Power(a, b) => a.toCNF ^ b.toCNF
  }
}

case class Multiplication(lhs: Ordinal, rhs: Ordinal) extends Ordinal

case class Sum(lhs: Ordinal, rhs: Ordinal) extends Ordinal

case class Power(lhs: Ordinal, rhs: Ordinal) extends Ordinal

case class W() extends Ordinal

case class NatNumber(i: Int) extends Ordinal
