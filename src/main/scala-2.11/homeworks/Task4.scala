package homeworks

import java.io.PrintWriter

import library.formalarithmetic._

import scala.io.Source._

/**
  * @author amir.
  */
object Task4 {
  val correct = List(1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  val incorrect = 1 to 11

  def main(args: Array[String]) {
    for (i <- correct) {
      println(i)
      val inputFileName = s"tests/HW4/correct$i.in"
      val outputFileName = s"res$i.out"
      val output = new PrintWriter(outputFileName)

      val lines = fromFile(inputFileName).getLines()
      val header = FormalArithmeticParser.parseHeader(lines.next())
      if (header.isEmpty) {
        println("Can't parse header")
        return
      }
      val expressions = lines.filter(!_.isEmpty).map(FormalArithmeticParser.parseExpression).toSeq
      if (expressions.exists(_.isEmpty)) {
        println("Can't parse some expressions")
        return
      }

      val d = new DeductionFA(header.get._1, header.get._2, expressions.map(_.get))
      try {
        val result = d.deduction()
        output.println(s"${result._1.mkString(",")}|-${result._2}")
        result._3.foreach(output.println)
      } catch {
        case e: FAException => output.println(e.getError)
      }
      output.close()
    }
  }
}
