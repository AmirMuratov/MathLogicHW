package library.formalarithmetic

/**
  * @author amir.
  */
sealed trait FAExpression {
  override def toString: String = {
    this match {
      case Conjunction(x, y) => "(" + x.toString + "&" + y.toString + ")"
      case Disjunction(x, y) => "(" + x.toString + "|" + y.toString + ")"
      case Implication(x, y) => "(" + x.toString + "->" + y.toString + ")"
      case Negate(x) => "!(" + x.toString + ")"
      case AnySubst(name, expr) => "(@" + name + "(" + expr.toString + "))"
      case ExistsSubst(name, expr) => "(?" + name + "(" + expr.toString + "))"
      case Predicate(name, vars) => name + (if (vars.nonEmpty) "(" + vars.mkString(",") + ")" else "")
      case EqualityPredicate(x, y) => "(" + x.toString + "=" + y.toString + ")"
    }
  }

  def ==(o: FAExpression): Boolean = FAExpression.equalsWithSubst(this, Map(), o, Map())

  override def hashCode() = hashCodeWithSubst(Map())

  def hashCodeWithSubst(subst: Map[String, Int]):Int = {
    this match {
      case Conjunction(x, y) => 13 * x.hashCodeWithSubst(subst) + 17 * y.hashCodeWithSubst(subst)
      case Disjunction(x, y) => 19 * x.hashCodeWithSubst(subst) + 23 * y.hashCodeWithSubst(subst)
      case Implication(x, y) => 29 * x.hashCodeWithSubst(subst) + 31 * y.hashCodeWithSubst(subst)
      case Negate(x) => 37 * x.hashCodeWithSubst(subst)
      case AnySubst(name, expr) => 47 * expr.hashCodeWithSubst(subst + (name -> subst.size))
      case ExistsSubst(name, expr) => 53 * expr.hashCodeWithSubst(subst + (name -> subst.size))
      case Predicate(name, vars) =>
        name.hashCode * 41 + vars.zipWithIndex.map(x => x._1.hashCodeWithSubst(subst) * Utils.pow(73, x._2)).sum
      case EqualityPredicate(x, y) => 61 * x.hashCodeWithSubst(subst) + 59 * y.hashCodeWithSubst(subst)
    }
  }

  def substitute(varToSubst: String, subst: Term): FAExpression = {
    this match {
      case Conjunction(x, y) => Conjunction(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
      case Disjunction(x, y) => Disjunction(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
      case Implication(x, y) => Implication(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
      case Negate(x) => Negate(x.substitute(varToSubst, subst))
      case a@AnySubst(name, expr) => if (name.equals(varToSubst)) a
      else
        AnySubst(name, expr.substitute(varToSubst, subst))
      case a@ExistsSubst(name, expr) => if (name.equals(varToSubst)) a
      else
        ExistsSubst(name, expr.substitute(varToSubst, subst))
      case Predicate(name, vars) => Predicate(name, vars.map(_.substitute(varToSubst, subst)))
      case EqualityPredicate(x, y) => EqualityPredicate(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
    }
  }

  def getVariables: Set[String] = {
    this match {
      case Conjunction(x, y) => x.getVariables ++ y.getVariables
      case Disjunction(x, y) => x.getVariables ++ y.getVariables
      case Implication(x, y) => x.getVariables ++ y.getVariables
      case Negate(x) => x.getVariables
      case a@AnySubst(name, expr) => expr.getVariables - name
      case a@ExistsSubst(name, expr) => expr.getVariables - name
      case Predicate(name, vars) => vars.flatMap(_.getVariables).toSet
      case EqualityPredicate(x, y) => x.getVariables ++ y.getVariables
    }
  }

  def canSubst(varName: String, vars: Set[String], curBounded: Set[String]): Boolean = {
    this match {
      case Conjunction(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
      case Disjunction(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
      case Implication(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
      case Negate(x) => x.canSubst(varName, vars, curBounded)
      case a@AnySubst(name, expr) => expr.canSubst(varName, vars, curBounded + name)
      case a@ExistsSubst(name, expr) => expr.canSubst(varName, vars, curBounded + name)
      case Predicate(name, predVars) => predVars.forall(_.canSubst(varName, vars, curBounded))
      case EqualityPredicate(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
    }
  }

  def entersFree(varToCheck: String): Boolean = {
    this match {
      case Conjunction(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
      case Disjunction(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
      case Implication(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
      case Negate(x) => x.entersFree(varToCheck)
      case a@AnySubst(name, expr) => !(name == varToCheck) && expr.entersFree(varToCheck)
      case a@ExistsSubst(name, expr) => !(name == varToCheck) && expr.entersFree(varToCheck)
      case Predicate(name, predVars) => predVars.exists(_.entersFree(varToCheck))
      case EqualityPredicate(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
    }
  }

}

case class Conjunction(x: FAExpression, y: FAExpression) extends FAExpression

case class Disjunction(x: FAExpression, y: FAExpression) extends FAExpression

case class Implication(x: FAExpression, y: FAExpression) extends FAExpression

case class Negate(x: FAExpression) extends FAExpression

case class AnySubst(varName: String, expression: FAExpression) extends FAExpression

case class ExistsSubst(varName: String, expression: FAExpression) extends FAExpression

case class Predicate(predicateName: String, vars: Seq[Term]) extends FAExpression

case class EqualityPredicate(x: Term, y: Term) extends FAExpression

object FAExpression {
  def equalsWithSubst(l: FAExpression, lSubst: Map[String, Int],
                      r: FAExpression, rSubst: Map[String, Int]): Boolean = {
    (l, r) match {
      case (Conjunction(x1, y1), Conjunction(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Disjunction(x1, y1), Disjunction(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Implication(x1, y1), Implication(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Negate(x1), Negate(x2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst)
      case (AnySubst(name1, expr1), AnySubst(name2, expr2)) =>
        equalsWithSubst(expr1, lSubst + (name1 -> lSubst.size), expr2, rSubst + (name2 -> rSubst.size))
      case (ExistsSubst(name1, expr1), ExistsSubst(name2, expr2)) =>
        equalsWithSubst(expr1, lSubst + (name1 -> lSubst.size), expr2, rSubst + (name2 -> rSubst.size))
      case (Predicate(name1, vars1), Predicate(name2, vars2)) => name1.equals(name2) &&
        vars1.length == vars2.length &&
        !vars1.zip(vars2).exists(x => !Term.equalsWithSubst(x._1, lSubst, x._2, rSubst))
      case (EqualityPredicate(x1, y1), EqualityPredicate(x2, y2)) =>
        Term.equalsWithSubst(x1, lSubst, x2, rSubst) && Term.equalsWithSubst(y1, lSubst, y2, rSubst)
      case (_, _) => false
    }
  }
}
