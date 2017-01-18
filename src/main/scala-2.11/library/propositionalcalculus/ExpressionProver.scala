package library.propositionalcalculus

import library.propositionalcalculus.ProvesForOperations._

/**
  * @author amir.
  */
class ExpressionProver(val e: Expression) {
  val error = new StringBuilder("Высказывание ложно при ")
  var failed = false

  def prove(): Either[List[Expression], String] = {
    val res = rec(e.getVariables.toList, Map(), List())
    if (failed)
      Right(error.toString())
    else
      Left(res)
  }

  def getProof(vars: Map[String, Boolean], exp: Expression): List[Expression] = {
    exp match {
      case Implication(x, y) =>
        val p1 = getProof(vars, x)
        val p2 = getProof(vars, y)
        p1 ::: p2 ::: makeSubst(impl(x.calculate(vars), y.calculate(vars)), x, y)
      case Disjunction(x, y) =>
        val p1 = getProof(vars, x)
        val p2 = getProof(vars, y)
        p1 ::: p2 ::: makeSubst(or(x.calculate(vars), y.calculate(vars)), x, y)
      case Conjunction(x, y) =>
        val p1 = getProof(vars, x)
        val p2 = getProof(vars, y)
        p1 ::: p2 ::: makeSubst(and(x.calculate(vars), y.calculate(vars)), x, y)
      case Negate(x) =>
        val p1 = getProof(vars, x)
        p1 ::: makeSubst(not(x.calculate(vars)), x, x)
      case _ =>
        List()
    }
  }

  def Lemma44(a: Expression, b: Expression): List[Expression] = {
    val proof = List(
      Implication(Implication(a, b), Implication(Implication(a, Negate(b)), Negate(a))),
      Implication(a, b),
      Implication(Implication(a, Negate(b)), Negate(a)),
      Implication(Negate(b), Implication(a, Negate(b))),
      Negate(b),
      Implication(a, Negate(b)),
      Negate(a)
    )
    val (newHyp, newExpr, newProof) = new Deduction(List(Implication(a, b), Negate(b)), Negate(a), proof).deduction()
    val (_, _, newProof2) = new Deduction(newHyp, newExpr, newProof).deduction()
    newProof2
  }

  def Lemma45(a: Expression): List[Expression] = {
    val FirstProof = Implication(a, Disjunction(a, Negate(a))) ::
      Lemma44(a, Disjunction(a, Negate(a))) :::
      List(Implication(Negate(Disjunction(a, Negate(a))), Negate(a)))
    val SecondProof =
      Implication(Negate(a), Disjunction(a, Negate(a))) ::
        Lemma44(Negate(a), Disjunction(a, Negate(a))) :::
        List(Implication(Negate(Disjunction(a, Negate(a))), Negate(Negate(a)))
        )
    val ThirdProof = List(
      Implication(Implication(Negate(Disjunction(a, Negate(a))), Negate(a)),
        Implication(Implication(Negate(Disjunction(a, Negate(a))), Negate(Negate(a))),
          Negate(Negate(Disjunction(a, Negate(a)))))),
      Implication(Implication(Negate(Disjunction(a, Negate(a))), Negate(Negate(a))),
        Negate(Negate(Disjunction(a, Negate(a))))),
      Negate(Negate(Disjunction(a, Negate(a)))),
      Implication(Negate(Negate(Disjunction(a, Negate(a)))), Disjunction(a, Negate(a))),
      Disjunction(a, Negate(a))
    )
    FirstProof ::: SecondProof ::: ThirdProof
  }


  def Lemma46(p: Expression, a: Expression, proof1: List[Expression], proof2: List[Expression]): List[Expression] = {
    val proof = List(
      Implication(Implication(p, a), Implication(Implication(Negate(p), a), Implication(Disjunction(p, Negate(p)), a))),
      Implication(Implication(Negate(p), a), Implication(Disjunction(p, Negate(p)), a)),
      Implication(Disjunction(p, Negate(p)), a),
      a
    )
    proof1 ::: proof2 ::: Lemma45(p) ::: proof
  }

  def rec(vars: List[String], assignedVars: Map[String, Boolean], hypothesis: List[Expression]): List[Expression] = {
    vars match {
      case x :: xs =>
        val p = PropVar(x)
        val proofTrue = rec(xs, assignedVars + (x -> true), p :: hypothesis)
        val proofFalse = rec(xs, assignedVars + (x -> false), Negate(p) :: hypothesis)
        val (_, _, deductedProofTrue) = new Deduction((p :: hypothesis).reverse, e, proofTrue).deduction()
        val (_, _, deductedProofFalse) = new Deduction((Negate(p) :: hypothesis).reverse,
          e, proofFalse).deduction()
        Lemma46(p, e, deductedProofTrue, deductedProofFalse)
      case Nil =>
        if (!e.calculate(assignedVars)) {
          if (!failed)
            error.append(assignedVars.toList.map(x => {
              x._1 + "=" + (if (x._2) "И" else "Л")
            }).mkString(","))
          failed = true
          List() //todo
        } else {
          getProof(assignedVars, e)
        }
    }
  }

}
