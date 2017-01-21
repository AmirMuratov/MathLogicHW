package library.formalarithmetic

import scala.io.BufferedSource

/**
  * @author amir.
  */
object ProofsForInferenceRules {
  val proof1 = getProof("proof1.txt")
  val proof2 = getProof("proof2.txt")
  val proof3 = getProof("proof3.txt")


  def makeSubst(expr: List[FAExpression], a: FAExpression, b: FAExpression, c: FAExpression) =
    expr.map(subst(_, a, b, c))

  private def subst(expr: FAExpression, a: FAExpression, b: FAExpression,
                    c: FAExpression): FAExpression = {
    expr match {
      case Conjunction(x, y) => Conjunction(subst(x, a, b, c), subst(y, a, b, c))
      case Disjunction(x, y) => Disjunction(subst(x, a, b, c), subst(y, a, b, c))
      case Implication(x, y) => Implication(subst(x, a, b, c), subst(y, a, b, c))
      case Negate(x) => Negate(subst(x, a, b, c))
      case Predicate(name, _) => name match {
        case "A" => a
        case "B" => b
        case "C" => c
        case _ => throw new IllegalStateException()
      }
      case _ => throw new IllegalStateException()
    }
  }

  private def getProof(s: String): List[FAExpression] = {
    new BufferedSource(getClass.getClassLoader.getResourceAsStream(s"proofs/$s"))
      .getLines().map(_.replaceAll("\\s", "")).map(FormalArithmeticParser.parseExpression).map(_.get).toList
  }
}
