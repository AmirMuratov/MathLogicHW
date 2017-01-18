package homeworks

import java.io.PrintWriter

import library.propositionalcalculus.{ExpressionParser, Deduction}

import scala.io.Source._

/**
  * @author amir.
  */
object Task2 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW2/contra.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val lines = fromFile(inputFileName).getLines()
    val header = ExpressionParser.parseHeader(lines.next())
    if (header.isEmpty) {
      println("Can't parse header")
      return
    }
    val expressions = lines.filter(!_.isEmpty).map(ExpressionParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }

    val d = new Deduction(header.get._1, header.get._2, expressions.map(_.get))
    val result = d.deduction()
    output.println(s"${result._1.mkString(",")}|-${result._2}")
    result._3.foreach(output.println(_))
    output.close()
  }
}
