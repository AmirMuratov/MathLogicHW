package library.formalarithmetic

/**
  * @author amir.
  */
object FAAxioms {

  def checkPCAxioms(expr: FAExpression): Option[Int] = {
    expr match {
      case Implication(a1, Implication(a2, a3)) if a1 == a3 => Some(1)
      case Implication(Implication(a1, a2), Implication(Implication(a3, Implication(a4, a5)), Implication(a6, a7)))
        if a1 == a3 && a3 == a6 && a2 == a4 && a5 == a7 => Some(2)
      case Implication(a1, Implication(a2, Conjunction(a3, a4))) if a1 == a3 && a2 == a4 => Some(3)
      case Implication(Conjunction(a1, a2), a3) if a1 == a3 => Some(4)
      case Implication(Conjunction(a1, a2), a3) if a2 == a3 => Some(5)
      case Implication(a1, Disjunction(a2, a3)) if a1 == a2 => Some(6)
      case Implication(a1, Disjunction(a2, a3)) if a1 == a3 => Some(7)
      case Implication(Implication(a1, a2), Implication(Implication(a3, a4), Implication(Disjunction(a5, a6), a7)))
        if a1 == a5 && a2 == a4 && a4 == a7 && a3 == a6 => Some(8)
      case Implication(Implication(a1, a2), Implication(Implication(a3, Negate(a4)), Negate(a5)))
        if a1 == a3 && a3 == a5 && a2 == a4 => Some(9)
      case Implication(Negate(Negate(a1)), a2) if a1 == a2 => Some(10)
      case _ => None
    }
  }

  def checkPredicateAxioms(expr: FAExpression): Option[Int] = {
    expr match {
      case _ => Some(11)
      case _ => Some(12)//todo
    }
  }

  def checkFAAxioms(expr: FAExpression): Option[Int] = {
    expr match {
      case Implication(EqualityPredicate(a1, a2), EqualityPredicate(PlusOne(a3), PlusOne(a4)))
        if a1 == a3 && a2 == a4 => Some(13)
      case Implication(EqualityPredicate(a1, a2), Implication(EqualityPredicate(a3, a4), EqualityPredicate(a5, a6)))
        if a1 == a3 && a2 == a5 && a4 == a6 => Some(14)
      case Implication(EqualityPredicate(PlusOne(a1), PlusOne(a2)), EqualityPredicate(a3, a4))
        if a1 == a3 && a2 == a4 => Some(15)
      case Negate(EqualityPredicate(PlusOne(a), Zero())) => Some(16)
      case EqualityPredicate(Sum(a1, PlusOne(a2)), PlusOne(Sum(a3, a4)))
        if a1 == a3 && a2 == a4 => Some(17)
      case EqualityPredicate(Sum(a1, Zero()), a2) if a1 == a2 => Some(18)
      case EqualityPredicate(Multiplication(a, Zero()), Zero()) => Some(19)
      case EqualityPredicate(Multiplication(a1, PlusOne(a2)), Sum(Multiplication(a3, a4), a5))
        if a1 == a3 && a1 == a5 && a2 == a4 => Some(20)
      case Implication(Conjunction(a1, AnySubst(name, Implication(a2, a3))), a4)
        if a2.entersFree(name) && a2.substitute(name, Zero()) == a1 &&
          a2.substitute(name, PlusOne(Variable(name))) == a3 && a2 == a4 => Some(21)
    }
  }

  def check(e: FAExpression): Option[Int] = {
    Seq(checkPCAxioms(e), checkPredicateAxioms(e), checkFAAxioms(e)).find(_.isDefined).getOrElse(None)
  }

  def isFreeForSubst(expr: FAExpression, subst: Term, varToSubst: String): Boolean =
    expr.canSubst(varToSubst, subst.getVariables, Set())


  def isSubstituted(varName: String, expr1: FAExpression, expr2: FAExpression): Either[String, Option[Term]] = {
    (expr1, expr2) match {
      case (Conjunction(x1, y1), Conjunction(x2, y2))
      => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Disjunction(x1, y1), Disjunction(x2, y2))
      => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Implication(x1, y1), Implication(x2, y2))
      => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Negate(x1), Negate(x2)) => isSubstituted(varName, x1, x2)
      case (AnySubst(name1, expr1), AnySubst(name2, expr2)) => ???
      case (ExistsSubst(name1, expr1), ExistsSubst(name2, expr2)) => ???
      case (Predicate(name1, vars1), Predicate(name2, vars2)) if name1 == name2 && vars1.length == vars2.length
      => check(vars1.zip(vars2).map((p) => isSubstituted(varName, p._1, p._2)))
      case (EqualityPredicate(x1, y1), EqualityPredicate(x2, y2))
      => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (_, _) => Left("err")
    }
  }

  def isSubstituted(varName: String, expr1: Term, expr2: Term): Either[String, Option[Term]] = {
    (expr1, expr2) match {
      case (Sum(x1, y1), Sum(x2, y2)) => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Multiplication(x1, y1), Multiplication(x2, y2))
      => check(isSubstituted(varName, x1, x2), isSubstituted(varName, y1, y2))
      case (Function(name1, vars1), Function(name2, vars2)) if name1 == name2 && vars1.length == vars2.length
      => check(vars1.zip(vars2).map((p) => isSubstituted(varName, p._1, p._2)))
      case (Zero(), Zero()) => Right(None)
      case (PlusOne(x1), PlusOne(x2)) => isSubstituted(varName, x1, x2)
      case (Variable(name), t) if name.equals(varName) => Right(Some(t))
      case (Variable(name1), Variable(name2)) => if (name1.equals(name2)) Right(None) else Left("err")
      case (_, _) => Left("err")
    }
  }

  def check(s: Seq[Either[String, Option[Term]]]): Either[String, Option[Term]] =
    s.fold(Right(None))(check)

  def check(x1: Either[String, Option[Term]], x2: Either[String, Option[Term]]):
  Either[String, Option[Term]] = {
    val s = Set(x1, x2)
    if (s.exists(_.isLeft)) return Left("err")
    if (s.exists(_.right.get.isEmpty)) {
      return s.find(_.right.getOrElse(None).isDefined).getOrElse(Right(None))
    }
    if (x1.right.get.get == x2.right.get.get) x1 else Left("err")
  }

}
