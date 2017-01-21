package library.formalarithmetic

/**
  * @author amir.
  */
sealed trait Term {
  override def toString: String = {
    this match {
      case Sum(x, y) => "(" + x.toString + "+" + y.toString + ")"
      case Multiplication(x, y) => "(" + x.toString + "*" + y.toString + ")"
      case Function(name, vars) => name + "(" + vars.mkString(",") + ")"
      case Variable(name) => name
      case Zero() => "0"
      case PlusOne(term) => term.toString + "'"
    }
  }

  def ==(o: Term): Boolean = {
    Term.equalsWithSubst(this, Map(), o, Map())
  }

  def hashCodeWithSubst(subst: Map[String, Int]):Int = {
    this match {
      case Sum(x, y) => x.hashCodeWithSubst(subst) * 17 + y.hashCodeWithSubst(subst) * 23
      case Multiplication(x, y) => x.hashCodeWithSubst(subst) * 29 + y.hashCodeWithSubst(subst) * 31
      case Function(name, vars) =>
        name.hashCode * 41 + vars.zipWithIndex.map(x => x._1.hashCodeWithSubst(subst) * Utils.pow(47, x._2)).sum
      case Variable(name) => if (subst.contains(name)) subst.get(name).get * 41 else name.hashCode * 41
      case Zero() => 53
      case PlusOne(term) => 13 * term.hashCodeWithSubst(subst)
    }
  }

  def substitute(varToSubst: String, subst: Term): Term = {
    this match {
      case Sum(x, y) => Sum(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
      case Multiplication(x, y) => Multiplication(x.substitute(varToSubst, subst), y.substitute(varToSubst, subst))
      case Function(funcName, vars) => Function(funcName, vars.map(_.substitute(varToSubst, subst)))
      case a@Variable(name) => if (name.equals(varToSubst)) subst else a
      case Zero() => Zero()
      case PlusOne(x) => PlusOne(x.substitute(varToSubst, subst))
    }
  }

  def getVariables: Set[String] = {
    this match {
      case Sum(x, y) => x.getVariables ++ y.getVariables
      case Multiplication(x, y) => x.getVariables ++ y.getVariables
      case Function(funcName, vars) => vars.flatMap(_.getVariables).toSet
      case Variable(name) => Set(name)
      case Zero() => Set()
      case PlusOne(x) => x.getVariables
    }
  }

  def canSubst(varName: String, vars: Set[String], curBounded: Set[String]): Boolean = {
    this match {
      case Sum(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
      case Multiplication(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
      case Function(funcName, funcVars) => funcVars.forall(_.canSubst(varName, vars, curBounded))
      case Variable(name) => if (!curBounded.contains(name) && name.equals(varName))
        vars.intersect(curBounded).isEmpty else true
      case Zero() => true
      case PlusOne(x) => x.canSubst(varName, vars, curBounded)
    }
  }

  def entersFree(varToCheck: String): Boolean = {
    this match {
      case Sum(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
      case Multiplication(x, y) => x.entersFree(varToCheck) || y.entersFree(varToCheck)
      case Function(name, funcVars) => funcVars.forall(_.entersFree(varToCheck))
      case Variable(name) => varToCheck.equals(name)
      case Zero() => false
      case PlusOne(x) => x.entersFree(varToCheck)
    }
  }
}

case class Sum(left: Term, right: Term) extends Term

case class Multiplication(left: Term, right: Term) extends Term

case class Function(funcName: String, vars: Seq[Term]) extends Term

case class Variable(name: String) extends Term

case class Zero() extends Term

case class PlusOne(x: Term) extends Term

object Term {
  def equalsWithSubst(l: Term, lSubst: Map[String, Int],
                      r: Term, rSubst: Map[String, Int]): Boolean = {
    (l, r) match {
      case (Sum(x1, y1), Sum(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Multiplication(x1, y1), Multiplication(x2, y2)) =>
        equalsWithSubst(x1, lSubst, x2, rSubst) && equalsWithSubst(y1, lSubst, y2, rSubst)
      case (Function(name1, vars1), Function(name2, vars2)) => name1.equals(name2) &&
        vars1.length == vars2.length && !vars1.zip(vars2).exists((x) =>
        !equalsWithSubst(x._1, lSubst, x._2, rSubst))
      case (Variable(name1), Variable(name2)) =>
        lSubst.contains(name1) && rSubst.contains(name2) && lSubst.get(name1) == rSubst.get(name2) ||
        !lSubst.contains(name1) && !rSubst.contains(name2) && name1.equals(name2)
      case (Zero(), Zero()) => true
      case (PlusOne(term1), PlusOne(term2)) => equalsWithSubst(term1, lSubst, term2, rSubst)
      case (_, _) => false
    }
  }
}



