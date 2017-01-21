package homeworks

import java.io.PrintWriter

import library.formalarithmetic.{DeductionFA, FAProveChecker, FormalArithmeticParser}

import scala.io.Source._

/**
  * @author amir.
  */
object Task4 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW4/correct1.in"
    val outputFileName = "res.out"
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
    val result = d.deduction()
    output.println(s"${result._1.mkString(",")}|-${result._2}")
    new FAProveChecker().ProveChecker(result._3).foreach(output.println)
    output.close()
  }
}
