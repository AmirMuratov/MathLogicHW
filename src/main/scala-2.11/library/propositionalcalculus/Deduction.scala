package library.propositionalcalculus

import scala.collection.mutable

/**
  * @author amir.
  */
class Deduction(hypothesis: Seq[Expression],
                provingExpr: Expression, prove: Seq[Expression]) {


  def deduction(): (Seq[Expression], Expression, List[Expression]) = {
    /*println("hypothesis:")
    hypothesis.foreach(println)
    println(s"expression:${provingExpr.toString}")
    println("proof:")
    prove.foreach(println)*/
    val splitedH = hypothesis.splitAt(hypothesis.length - 1)
    val a = splitedH._2.head
    val newHypothesis = splitedH._1
    val resultProve = new mutable.ArrayBuffer[Expression]()
    val mp = new ModusPonensChecker()
    for (e <- prove) {
      if (AxiomChecker.check(e).isDefined || newHypothesis.contains(e)) {
        resultProve +=(e,
          Implication(e, Implication(a, e)),
          Implication(a, e)
          )
      } else if (e == a) {
        resultProve +=(Implication(a, Implication(a, a)),
          Implication(Implication(a, Implication(a, a)),
            Implication(Implication(a, Implication(Implication(a, a), a)), Implication(a, a))),
          Implication(Implication(a, Implication(Implication(a, a), a)), Implication(a, a)),
          Implication(a, Implication(Implication(a, a), a)),
          Implication(a, a)
          )
      } else {
        val k = mp.check(e).get._1
        resultProve +=(Implication(Implication(a, k), Implication(Implication(a, Implication(k, e)), Implication(a, e))),
          Implication(Implication(a, Implication(k, e)), Implication(a, e)),
          Implication(a, e)
          )
      }
      mp.addProved(e)
    }
    (newHypothesis, Implication(a, provingExpr), resultProve.toList)
  }

}
