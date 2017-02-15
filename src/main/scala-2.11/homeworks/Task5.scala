package homeworks

import java.io.PrintWriter

import library.kripkemodels.Kripke
import library.propositionalcalculus.ExpressionParser

import scala.io.Source.fromFile

/**
  * Created by amir on 10.02.17.
  */
object Task5 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW5/true1.in"
    val outputFileName = "res.out"
    val output = new PrintWriter(outputFileName)

    val expressions = fromFile(inputFileName).getLines().filter(!_.isEmpty).map(ExpressionParser.parseExpression).toSeq
    if (expressions.exists(_.isEmpty)) {
      println("Can't parse some expressions")
      return
    }

    new Kripke().findModel(expressions.head.get) match {
      case None => output.println("Формула общезначима")
      case Some(x) => output.println(x)
    }
    output.close()
  }
}
