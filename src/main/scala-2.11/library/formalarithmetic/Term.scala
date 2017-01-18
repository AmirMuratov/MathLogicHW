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
      case Zero() => "0"
      case PlusOne(term) => "(" + term.toString + "')"
    }
  }

  def ==(o: Term): Boolean = {
    (this, o) match {
      case (Sum(x1, y1), Sum(x2, y2)) => x1 == x2 && y1 == y2
      case (Multiplication(x1, y1), Multiplication(x2, y2)) => x1 == x2 && y1 == y2
      case (Function(name1, vars1), Function(name2, vars2)) => name1.equals(name2) &&
        vars1.length == vars2.length && !vars1.zip(vars2).exists((x) => !(x._1 == x._2))
      case (Zero(), Zero()) => true
      case (PlusOne(term1), PlusOne(term2)) => term1 == term2
      case (_, _) => false
    }
  }

  override def hashCode() = {
    this match {
      case Sum(x, y) => ???
      case Multiplication(x, y) => ???
      case Function(name, vars) => ???
      case Zero() => ???
      case PlusOne(term) => ???
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
    case Sum(x, y) => x.getVariables ++ y.getVariables
    case Multiplication(x, y) => x.getVariables ++ y.getVariables
    case Function(funcName, vars) => vars.flatMap(_.getVariables)
    case Variable(name) => Set(name)
    case Zero() => Set()
    case PlusOne(x) => x.getVariables
  }

  def canSubst(varName: String, vars: Set[String], curBounded: Set[String]): Boolean = {
    case Sum(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
    case Multiplication(x, y) => x.canSubst(varName, vars, curBounded) && y.canSubst(varName, vars, curBounded)
    case Function(funcName, funcVars) => funcVars.forall(_.canSubst(varName, vars, curBounded))
    case Variable(name) => if (name.equals(varName)) vars.intersect(curBounded).isEmpty else true
    case Zero() => true
    case PlusOne(x) => x.canSubst(varName, vars, curBounded)
  }
}

case class Sum(left: Term, right: Term) extends Term

case class Multiplication(left: Term, right: Term) extends Term

case class Function(funcName: String, vars: Seq[Term]) extends Term

case class Variable(name: String) extends Term

case class Zero() extends Term

case class PlusOne(x: Term) extends Term



