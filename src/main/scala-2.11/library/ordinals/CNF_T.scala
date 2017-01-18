package library.ordinals

/**
  * @author amir.
  */
sealed trait CNF_T extends Ordered[CNF_T] {

  override def compare(that: CNF_T): Int = (this, that) match {
    case (Atom(i), Atom(j)) => i compare j
    case (_, Atom(_)) => 1
    case (Atom(_), _) => -1
    case _ if firstExp != that.firstExp => firstExp compare that.firstExp
    case _ if firstCoeff != that.firstCoeff => firstCoeff compare that.firstCoeff
    case _ => rest compare that.rest
  }

  def +(that: CNF_T): CNF_T = (this, that) match {
    case (Atom(i), Atom(j)) => Atom(i + j)
    case _ if firstExp < that.firstExp => that
    case _ if firstExp == that.firstExp => (firstExp, firstCoeff + that.firstCoeff) ::: that.rest
    case _ => (firstExp, firstCoeff) ::: (rest + that)
  }

  def -(that: CNF_T): CNF_T = (this, that) match {
    case (Atom(i), Atom(j)) => if (i < j) Atom(0) else Atom(i - j)
    case _ if firstExp < that.firstExp => Atom(0)
    case _ if firstExp > that.firstExp => this
    case _ if firstCoeff < that.firstCoeff => Atom(0)
    case _ if firstCoeff > that.firstCoeff => (firstExp, firstCoeff - that.firstCoeff) ::: rest
    case _ => rest - that.rest
  }

  def *(that: CNF_T): CNF_T = (this, that) match {
    case (a, b) if a == Atom(0) || b == Atom(0) => Atom(0)
    case (Atom(i), Atom(j)) => Atom(i * j)
    case (_, Atom(j)) => (firstExp, firstCoeff * that) ::: rest
    case _ => (firstExp + that.firstExp, that.firstCoeff) ::: (this * that.rest)
  }

  def ^(that: CNF_T): CNF_T = (this, that) match {
    case _ if this == Atom(1) || that == Atom(0) => Atom(1)
    case _ if this == Atom(0) => Atom(0)
    case (Atom(i), Atom(j)) => Atom(math.pow(i, j).toInt)
    case (a@Atom(_), _) => exp1(a, that)
    case (_, a@Atom(_)) => exp3(this, a)
    case _ => exp4(this, that)
  }

  def exp1(a: Atom, pow: CNF_T): CNF_T = pow match {
    case _ if pow.firstExp == Atom(1) => val atom = pow.firstExp match {
      case a@Atom(_) => a
      case _ => Atom(0)
    }
      (pow.firstCoeff, Atom(math.pow(a.i, atom.i).toInt)) ::: Atom(0)
    case _ if (pow.rest match {
      case Atom(_) => true
      case _ => false
    }) => val atom = pow.rest match {
      case a@Atom(_) => a
      case _ => Atom(0)
    }
      ((pow.firstExp - Atom(1), pow.firstCoeff) ::: Atom(0)) :: Atom(math.pow(a.i, atom.i).toInt) :: Atom(0)
    case _ => val c = exp1(a, pow.rest)
      (pow.firstExp - Atom(1), Atom(1)) ::: c.firstExp :: c.firstCoeff :: Atom(0)
  }

  def exp2(a: CNF_T, pow: Atom): CNF_T = pow match {
    case _ if pow == Atom(1) => a
    case _ => ((a.firstExp * (pow - Atom(1)), Atom(1)) ::: Atom(0)) * a
  }

  def exp3(a: CNF_T, pow: Atom): CNF_T = pow match {
    case _ if pow == Atom(0) => Atom(1)
    case _ if pow == Atom(1) => a
    case _ if a.isLimit => exp2(a, pow)
    case Atom(n) => exp3(a, Atom(n - 1)) * a
  }

  def exp4(a: CNF_T, pow: CNF_T): CNF_T = ((a.firstExp * pow.limitpart, Atom(1)) ::: Atom(0)) * exp3(a, pow.natPart)

  def first: CNF_T = this match {
    case Atom(_) => Atom(0)
    case CNF((x, _)) => x
  }

  def rest: CNF_T = this match {
    case Atom(_) => Atom(0)
    case CNF((_, x)) => x
  }

  def firstExp: CNF_T = this match {
    case Atom(_) => Atom(0)
    case _ => first.first
  }

  def firstCoeff: CNF_T = this match {
    case Atom(i) => this
    case _ => first.rest
  }

  def isLimit: Boolean = this match {
    case Atom(i) => i == BigInt(0)
    case _ => rest.isLimit
  }

  def natPart: Atom = this match {
    case a@Atom(_) => a
    case _ => rest.natPart
  }


  def limitpart: CNF_T = this match {
    case Atom(_) => Atom(0)
    case _ => first :: rest.limitpart
  }

  def :::(p: (CNF_T, CNF_T)): CNF = CNF(p._1, p._2) :: this

  def ::(c: CNF_T): CNF = CNF(c, this)
}

case class CNF(cnf: (CNF_T, CNF_T)) extends CNF_T

case class Atom(i: Int) extends CNF_T