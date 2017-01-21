package library.formalarithmetic

import scala.collection.mutable

import library.formalarithmetic.ProofsForInferenceRules._

/**
  * @author amir.
  */
class DeductionFA(hypothesis: Seq[FAExpression],
                  provingExpr: FAExpression, prove: Seq[FAExpression]) {


  def deduction(): (Seq[FAExpression], FAExpression, List[FAExpression]) = {
    /*println("hypothesis:")
    hypothesis.foreach(println)
    println(s"FAExpression:${provingExpr.toString}")
    println("proof:")
    prove.foreach(println)*/
    val splitedH = hypothesis.splitAt(hypothesis.length - 1)
    val a = splitedH._2.head
    val newHypothesis = splitedH._1
    val resultProve = new mutable.ArrayBuffer[FAExpression]()
    val rulesCheck = new InferenceRulesCheck()
    for (e <- prove) {
      if (FAAxioms.check(e).isDefined || newHypothesis.contains(e)) {
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
      } else if (rulesCheck.checkPCRAny(e).isDefined) {
        // e == A -> Any x(B)
        // and exists A -> B (a -> A -> B)
        // should prove a -> A -> Any x(B)
        e match {
          case all@Implication(l, an@AnySubst(x, r)) =>
            makeSubst(proof2, a, l, r).foreach(resultProve += _)
            resultProve +=(Implication(Conjunction(a, l), r),
              Implication(Conjunction(a, l), an))
            makeSubst(proof3, a, l, an).foreach(resultProve += _)
            resultProve += Implication(a, all)
          case _ => throw new IllegalStateException()
        }
      } else if (rulesCheck.checkPCRExists(e).isDefined) {
        // e == Exists x(A) -> B
        // and exists A -> B (a -> A -> B)
        // should prove a -> Exists x(A) -> B
        e match {
          case all@Implication(ex@ExistsSubst(x, l), r) =>
            makeSubst(proof1, a, l, r).foreach(resultProve += _)
            resultProve +=(Implication(l, Implication(a, r)),
              Implication(ex, Implication(a, r)))
            makeSubst(proof1, ex, a, r).foreach(resultProve += _)
            resultProve += Implication(a, all)
          case _ => throw new IllegalStateException()
        }
      } else {
        val mp = rulesCheck.checkMP(e)
        if (mp.isDefined) {
          val k = mp.get._1
          resultProve +=(Implication(Implication(a, k), Implication(Implication(a, Implication(k, e)), Implication(a, e))),
            Implication(Implication(a, Implication(k, e)), Implication(a, e)),
            Implication(a, e)
            )
        } else {
          throw new IllegalStateException()
        }
      }
      rulesCheck.addProved(e)
    }
    (newHypothesis, Implication(a, provingExpr), resultProve.toList)
  }
}
