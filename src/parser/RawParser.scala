package parser

import scala.collection.immutable.ListMap
import scala.collection.mutable

object RawParser {

  def parse(input: String): ParsedInput = {

    val iterator = new InputIterator(input)
    val functionName = readUntil(iterator, _ != '(')

    var params = ListMap[String, String]()

    while(iterator.hasNext) {
      val name = readUntil(iterator, _ != '=')
      val value = readUntil(iterator, (ch) => ch != ',' && ch != ')')
      params += name -> value
    }

    ParsedInput(functionName, params.toMap)
  }

  def readUntil(iterator: InputIterator, predicate: Char => Boolean): String = {
    val sb = new mutable.StringBuilder

    var ch: Char = iterator.next
    while (predicate(ch)) {
      sb.append(ch)
      ch = iterator.next
    }
    sb.toString()
  }


  def main(args: Array[String]): Unit = {

    val toParse = "React(generation=0,time=100,view=__W_W_W__,energy=100)"
    val parsed = ParsedInput("React", Map("generation" -> "0",
                                          "time" -> "100",
                                          "view" -> "__W_W_W__",
                                          "energy" -> "100"))

    if(RawParser.parse(toParse) == parsed) {
      println("OK")
    } else {
      println("KO " + parsed)
    }
  }

}
