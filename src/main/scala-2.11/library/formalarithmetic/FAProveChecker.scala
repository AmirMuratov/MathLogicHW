package library.formalarithmetic

import scala.collection.mutable

/**
  * @author amir.
  */
class FAProveChecker {
  private val proved = mutable.HashMap[FAExpression, Int]()
  //private val mpChecker = new ModusPonensChecker()

  /*def ProveFAExpression(expr: (FAExpression, Int)): String = {
    proved.put(expr._1, expr._2 + 1)
    mpChecker.addProved(expr._1)
    if (AxiomChecker.check(expr._1).isDefined) return s"Сх. Акс. ${AxiomChecker.check(expr._1).get}"
    val mp = mpChecker.check(expr._1)
    if (mp.isDefined) return s"M.P. ${proved.get(mp.get._1).get}, ${proved.get(mp.get._2).get}"
    "Не доказано"
  }

  def ProveChecker(FAExpressions: List[FAExpression]) = {
    FAExpressions.zipWithIndex.map(x => "(" + (x._2 + 1) + ") " + x._1 + " (" + ProveFAExpression(x) + ")")
  }
*/
}
