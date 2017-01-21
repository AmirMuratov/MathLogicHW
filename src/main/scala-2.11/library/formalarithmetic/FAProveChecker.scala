package library.formalarithmetic

import scala.collection.mutable

/**
  * @author amir.
  */
class FAProveChecker {
  private val proved = mutable.HashMap[FAExpression, Int]()
  private val rulesChecker = new InferenceRulesCheck()

  def ProveFAExpression(expr: (FAExpression, Int)): String = {
    proved.put(expr._1, expr._2 + 1)
    rulesChecker.addProved(expr._1)
    if (FAAxioms.check(expr._1).isDefined) return s"Сх. Акс. ${FAAxioms.check(expr._1).get}"
    val mp = rulesChecker.checkMP(expr._1)
    if (mp.isDefined) return s"M.P. ${proved.get(mp.get._1).get}, ${proved.get(mp.get._2).get}"
    val pcrAny = rulesChecker.checkPCRAny(expr._1)
    if (pcrAny.isDefined) return s"Predicate calculus inference rule any with expr. ${proved.get(pcrAny.get).get}"
    val pcrExists = rulesChecker.checkPCRAny(expr._1)
    if (pcrExists.isDefined) return s"Predicate calculus inference rule exists with expr. ${proved.get(pcrExists.get).get}"
    "Не доказано"
  }

  def ProveChecker(FAExpressions: List[FAExpression]) = {
    FAExpressions.zipWithIndex.map(x => "(" + (x._2 + 1) + ") " + x._1 + " (" + ProveFAExpression(x) + ")")
  }

}
