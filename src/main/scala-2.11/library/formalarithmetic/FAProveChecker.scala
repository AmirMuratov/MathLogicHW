package library.formalarithmetic

import scala.collection.mutable

/**
  * @author amir.
  */
class FAProveChecker {
  private val proved = mutable.HashMap[FAExpression, Int]()
  private val rulesChecker = new InferenceRulesCheck()

  def ProveFAExpression(hypothesis: Set[FAExpression], expr: (FAExpression, Int)): String = {
    proved.put(expr._1, expr._2 + 1)
    rulesChecker.addProved(expr._1)
    if (hypothesis.contains(expr._1)) return "Предположение"
    if (FAAxioms.check(expr._1).isDefined) return s"Сх. Акс. ${FAAxioms.check(expr._1).get}"
    val mp = rulesChecker.checkMP(expr._1)
    if (mp.isDefined) return s"M.P. ${proved.get(mp.get._1).get}, ${proved.get(mp.get._2).get}"
    val pcrAny = rulesChecker.checkPCRAny(expr._1)
    if (pcrAny.isDefined) return s"Правило вывода ИП для квантора любой из выр. ${proved.get(pcrAny.get).get}"
    val pcrExists = rulesChecker.checkPCRExists(expr._1)
    if (pcrExists.isDefined) return s"Правило вывода ИП для квантора существует из выр. ${proved.get(pcrExists.get).get}"
    "Не доказано"
  }

  def ProveChecker(hypothesis: Seq[FAExpression], FAExpressions: List[FAExpression]) = {
    val hypothesisSet = hypothesis.toSet
    FAExpressions.zipWithIndex.map(x => "(" + (x._2 + 1) + ") " + x._1 + " (" + ProveFAExpression(hypothesisSet, x) + ")")
  }

}
