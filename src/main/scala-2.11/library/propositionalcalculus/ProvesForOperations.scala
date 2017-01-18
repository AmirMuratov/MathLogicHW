package library.propositionalcalculus

import scala.io.BufferedSource

/**
  * @author amir.
  */
object ProvesForOperations {
  val or = Map(
    (true, true) -> getProof("or11"),
    (true, false) -> getProof("or10"),
    (false, true) -> getProof("or01"),
    (false, false) -> getProof("or00"))
  val and = Map(
    (true, true) -> getProof("and11"),
    (true, false) -> getProof("and10"),
    (false, true) -> getProof("and01"),
    (false, false) -> getProof("and00"))
  val impl = Map(
    (true, true) -> getProof("impl11"),
    (true, false) -> getProof("impl10"),
    (false, true) -> getProof("impl01"),
    (false, false) -> getProof("impl00"))
  val not = Map(
    true -> getProof("not1"),
    false -> getProof("not0"))


  def makeSubst(expr: List[Expression], a: Expression, b: Expression) = {
    expr.map(subst(_, a, b))
  }

  private def subst(expression: Expression, a: Expression, b: Expression): Expression = {
    expression match {
      case Conjunction(x, y) => Conjunction(subst(x, a, b), subst(y, a, b))
      case Disjunction(x, y) => Disjunction(subst(x, a, b), subst(y, a, b))
      case Implication(x, y) => Implication(subst(x, a, b), subst(y, a, b))
      case Negate(x) => Negate(subst(x, a, b))
      case PropVar(name) => name match {
        case "A" => a
        case "B" => b
        case _ => throw new IllegalStateException()
      }
    }
  }

  private def getProof(s: String): List[Expression] = {
    new BufferedSource(getClass.getClassLoader.getResourceAsStream(s"proofs/$s"))
      .getLines().map(_.replaceAll("\\s", "")).map(ExpressionParser.parseExpression).map(_.get).toList
  }
}
