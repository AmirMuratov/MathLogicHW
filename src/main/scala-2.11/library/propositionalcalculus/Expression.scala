package library.propositionalcalculus

/**
  * Created by amir on 04.04.16.
  */
trait Expression {
  override def toString: String = {
    this match {
      case Conjunction(x, y) => "(" + x.toString + "&" + y.toString + ")"
      case Disjunction(x, y) => "(" + x.toString + "|" + y.toString + ")"
      case Implication(x, y) => "(" + x.toString + "->" + y.toString + ")"
      case Negate(x) => "!(" + x.toString + ")"
      case PropVar(name) => name
    }
  }

  def ==(o: Expression): Boolean = {
    (this, o) match {
      case (Conjunction(x1, y1), Conjunction(x2, y2)) => x1 == x2 && y1 == y2
      case (Disjunction(x1, y1), Disjunction(x2, y2)) => x1 == x2 && y1 == y2
      case (Implication(x1, y1), Implication(x2, y2)) => x1 == x2 && y1 == y2
      case (Negate(x1), Negate(x2)) => x1 == x2
      case (PropVar(x1), PropVar(x2)) => x1.equals(x2)
      case (_, _) => false
    }
  }

  def getVariables: Set[String] = {
    this match {
      case Conjunction(x, y) => x.getVariables ++ y.getVariables
      case Disjunction(x, y) => x.getVariables ++ y.getVariables
      case Implication(x, y) => x.getVariables ++ y.getVariables
      case Negate(x) => x.getVariables
      case PropVar(name) => Set(name)
    }
  }

  def calculate(vars: Map[String, Boolean]): Boolean = {
    this match {
      case Conjunction(x, y) => x.calculate(vars) & y.calculate(vars)
      case Disjunction(x, y) => x.calculate(vars) | y.calculate(vars)
      case Implication(x, y) => !x.calculate(vars) | y.calculate(vars)
      case Negate(x) => !x.calculate(vars)
      case PropVar(name) => vars.get(name).get
    }
  }

  override def hashCode() = {
      this match {
          case Conjunction(x, y) => 13 * x.hashCode() + 17 * y.hashCode()
          case Disjunction(x, y) => 19 * x.hashCode() + 23 * y.hashCode()
          case Implication(x, y) => 29 * x.hashCode() + 31 * y.hashCode()
          case Negate(x) => 37 * x.hashCode()
          case PropVar(name) => name.hashCode()
      }
  }
}

case class Conjunction(x: Expression, y: Expression) extends Expression

case class Disjunction(x: Expression, y: Expression) extends Expression

case class Implication(x: Expression, y: Expression) extends Expression

case class Negate(x: Expression) extends Expression

case class PropVar(name: String) extends Expression