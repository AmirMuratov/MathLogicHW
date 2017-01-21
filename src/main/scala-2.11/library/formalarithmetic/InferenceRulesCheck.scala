package library.formalarithmetic

import scala.collection.mutable

/**
  * @author amir.
  */
class InferenceRulesCheck {
  private val proved = mutable.HashSet[FAExpression]()
  private val canBeProvedByMP = mutable.HashMap[FAExpression, mutable.HashSet[FAExpression]]()

  def checkMP(e: FAExpression): Option[(FAExpression, FAExpression)] = {
    if (!canBeProvedByMP.contains(e))
      return None
    for (expr <- canBeProvedByMP.get(e).get) {
      if (proved.contains(expr))
        return Some(expr, Implication(expr, e))
    }
    None
  }

  def checkPCRAny(e: FAExpression): Option[FAExpression] = {
    e match {
      case Implication(x, AnySubst(name, y)) if proved.contains(Implication(x, y))
      => if (!x.entersFree(name)) Some(Implication(x, y)) else throw new VarEnterFree(0, name, x)
      case _ => None
    }
  }

  def checkPCRExists(e: FAExpression): Option[FAExpression] = {
    e match {
      case Implication(ExistsSubst(name, x), y) if proved.contains(Implication(x, y))
      => if (!y.entersFree(name)) Some(Implication(x, y)) else throw new VarEnterFree(0, name, y)
      case _ => None
    }

  }

  def addProved(expr: FAExpression) = {
    expr match {
      case a@Implication(x, y) =>
        if (!canBeProvedByMP.contains(y))
          canBeProvedByMP.put(y, new mutable.HashSet[FAExpression])
        canBeProvedByMP.get(y).get.add(x)
      case _ =>
    }
    proved.add(expr)
  }
}
