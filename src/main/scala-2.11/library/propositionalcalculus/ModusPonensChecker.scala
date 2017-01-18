package library.propositionalcalculus

import scala.collection.mutable

/**
  * @author amir.
  */
class ModusPonensChecker {
  private val proved = mutable.HashSet[Expression]()
  private val canBeProvedByMP = mutable.HashMap[Expression, mutable.HashSet[Expression]]()

  def check(e: Expression): Option[(Expression, Expression)] = {
    if (!canBeProvedByMP.contains(e))
      return None
    for (expr <- canBeProvedByMP.get(e).get) {
      if (proved.contains(expr))
        return Some(expr, Implication(expr, e))
    }
    None
  }

  def addProved(expr: Expression) = {
    expr match {
      case Implication(x, y) =>
        if (!canBeProvedByMP.contains(y))
          canBeProvedByMP.put(y, new mutable.HashSet[Expression])
        canBeProvedByMP.get(y).get.add(x)
      case _ =>
    }
    proved.add(expr)
  }
}
