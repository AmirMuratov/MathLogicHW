package homeworks

import java.io.PrintWriter

import library.ordinals.OrdinalParser

import scala.io.Source._

/**
  * @author amir.
  */
object Task8 {
  def main(args: Array[String]) {
    val inputFileName = "tests/HW8/equal7.in"

    val ordinals = OrdinalParser.parse2Ordinals(fromFile(inputFileName).getLines().next().replaceAll("\\s", ""))
    if (ordinals.isEmpty) {
      println("Can't parse ordinals")
      return
    }

    if (ordinals.get._1.toCNF == ordinals.get._2.toCNF)
      println("Равны")
    else
      println("Не равны")
  }
}
