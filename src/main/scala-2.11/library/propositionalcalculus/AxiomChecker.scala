package library.propositionalcalculus

/**
  * @author amir.
  */
object AxiomChecker {
  def check(e: Expression): Option[Int] = {
    e match {
      case Implication(a1, Implication(a2, a3)) if a1 == a3 => Some(1)
      case Implication(Implication(a1, a2), Implication(Implication(a3, Implication(a4, a5)), Implication(a6, a7)))
        if a1 == a3 && a3 == a6 && a2 == a4 && a5 == a7 => Some(2)
      case Implication(a1, Implication(a2, Conjunction(a3, a4))) if a1 == a3 && a2 == a4 => Some(3)
      case Implication(Conjunction(a1, a2), a3) if a1 == a3 => Some(4)
      case Implication(Conjunction(a1, a2), a3) if a2 == a3 => Some(5)
      case Implication(a1, Disjunction(a2, a3)) if a1 == a2 => Some(6)
      case Implication(a1, Disjunction(a2, a3)) if a1 == a3 => Some(7)
      case Implication(Implication(a1, a2), Implication(Implication(a3, a4), Implication(Disjunction(a5, a6), a7)))
        if a1 == a5 && a2 == a4 && a4 == a7 && a3 == a6 => Some(8)
      case Implication(Implication(a1, a2), Implication(Implication(a3, Negate(a4)), Negate(a5)))
        if a1 == a3 && a3 == a5 && a2 == a4 => Some(9)
      case Implication(Negate(Negate(a1)), a2) if a1 == a2 => Some(10)
      case _ => None
    }
  }

  //def ifModusPonens(a: Expression, aImplb: Expression, b: Expression) = {
  //  aImplb match {
  //    case Implication(x, y) => x == a & y == b
  //    case default => false
  //  }
  //}
}
