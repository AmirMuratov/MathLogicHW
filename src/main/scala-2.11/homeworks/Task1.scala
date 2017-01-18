package homeworks

import java.io.PrintWriter

import library.propositionalcalculus.{ExpressionParser, ProveChecker}

import scala.io.Source._

/**
  * @author amir.
  */
object Task1 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW1/wrong1.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(ExpressionParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }

    new ProveChecker().checkProve(expressions.map(_.get).toList).foreach(output.println)
    output.close()
  }
}
