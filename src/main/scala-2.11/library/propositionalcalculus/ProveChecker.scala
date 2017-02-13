package library.propositionalcalculus

import scala.collection.mutable

/**
  * Created by amir on 05.04.16.
  */
class ProveChecker {
  private val proved = mutable.HashMap[Expression, Int]()
  private val mpChecker = new ModusPonensChecker()

  def ProveExpression(expr: (Expression, Int)): String = {
    proved.put(expr._1, expr._2 + 1)
    mpChecker.addProved(expr._1)
    if (AxiomChecker.check(expr._1).isDefined) return s"Сх. Акс. ${AxiomChecker.check(expr._1).get}"
    val mp = mpChecker.check(expr._1)
    if (mp.isDefined) return s"M.P. ${proved(mp.get._1)}, ${proved(mp.get._2)}"
    "Не доказано"
  }

  def checkProve(expressions: List[Expression]) = {
    expressions.zipWithIndex.map(x => "(" + (x._2 + 1) + ") " + x._1 + " (" + ProveExpression(x) + ")")
  }

}
