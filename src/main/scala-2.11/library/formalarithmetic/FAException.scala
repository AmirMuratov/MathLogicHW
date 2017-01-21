package library.formalarithmetic

/**
  * @author amir.
  */
sealed trait FAException extends Exception {
  def getError = {
    s"Вывод некорректен начиная с формулы номер <${getNumber + 1}>: $getText"
  }
  def getNumber: Int
  def getText: String
}

class WrongInference(number: Int, expr: FAExpression) extends FAException {
  def getNumber = number
  def getText = s"выражение $expr не является ни аксиомой, ни правилом вывода"
}

class TermNotFreeForSubst(var number: Int, termX: Term, exprY: FAExpression, a: String) extends FAException {
  def getNumber = number
  def getText = s"терм $termX не свободен для подстановки в формулу $exprY вместо переменной $a"
  def setN(n: Int) = {
    number = n
  }
}

class VarEnterFree(var number: Int, a: String, exprX: FAExpression) extends FAException {
  def getNumber = number
  def getText = s"переменная $a входит свободно в формулу $exprX"
  def setN(n: Int) = {
    number = n
  }
}

class UsageOfVarThatEntersHyp(number: Int, RuleOrAx: String, a: String, hyp: FAExpression)
  extends FAException {
  def getNumber = number
  def getText = s"используется $RuleOrAx с квантором по переменной $a входящей " +
    s"свободно в допущение $hyp"
}