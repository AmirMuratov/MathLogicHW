package library.formalarithmetic

/**
  * @author amir.
  */
object Utils {
  def pow(a: Int, b: Int): Int = {
    b match {
      case 0 => 1
      case 1 => a
      case x if x % 2 == 0 =>
        val p = pow(a, x / 2)
        p * p
      case x => pow(a, x - 1) * a
    }
  }
}
