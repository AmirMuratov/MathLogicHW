package homeworks

import java.io.PrintWriter

import library.propositionalcalculus.{ExpressionParser, ExpressionProver, ProveChecker}

import scala.io.Source._

/**
  * @author amir.
  */
object Task3 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW3/true7.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)
    val expr = ExpressionParser.parseExpression(fromFile(inputFileName).getLines().next())
    if (expr.isEmpty) {
      println("Can't parse expression")
      return
    }
    new ExpressionProver(expr.get).prove() match {
      case Left(proof) =>
        new ProveChecker().checkProve(proof).foreach(output.println)
      case Right(s) => output.println(s)
    }
    output.close()
  }
}
