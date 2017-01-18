package library.formalarithmetic

/**
  * @author amir.
  */
object FAAxioms {
  def checkPredicateAxioms(expr: FAExpression): Option[Int] = {

  }

  //FAaxioms can be loaded from prop calc

  def isFreeForSubst(expr: FAExpression, subst: Term, varToSubst: String): Boolean =
   expr.canSubst(varToSubst, subst.getVariables, Set())

  def EntersFree(varToCheck: String, expr: FAExpression): Boolean = {
    
  }

  def isSubstituted(varName: String, expr1: FAExpression, expr2: FAExpression): Option[Term] = {
    (expr1, expr2) match {
      case (Conjunction(x1, y1), Conjunction(x2, y2)) => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Disjunction(x1, y1), Disjunction(x2, y2)) => x1 == x2 && y1 == y2
      case (Implication(x1, y1), Implication(x2, y2)) => x1 == x2 && y1 == y2
      case (Negate(x1), Negate(x2)) => x1 == x2
      case (AnySubst(name1, expr1), AnySubst(name2, expr2)) => ???
      case (ExistsSubst(name1, expr1), ExistsSubst(name2, expr2)) => ???
      case (Predicate(name1, vars1), Predicate(name2, vars2)) => name1.equals(name2) &&
        vars1.length == vars2.length && !vars1.zip(vars2).exists((x) => !(x._1 == x._2))
      case (EqualityPredicate(x1, y1), EqualityPredicate(x2, y2)) => x1 == x2 && y1 == y2
      case (_, _) => false
    }
  }

}
