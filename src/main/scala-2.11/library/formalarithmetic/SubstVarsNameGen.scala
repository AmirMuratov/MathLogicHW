package library.formalarithmetic

/**
  * @author amir.
  */
object SubstVarsNameGen {
  var i = 0
  val pref = "####"
  def getNext: String = {
    i += 1
    pref + i.toString
  }
}
