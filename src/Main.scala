import data.XY
import function.{ReactFunction, GoodbyeFunction, WelcomeFunction}
import parser.InputParser

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {

  def respond(input: String):String = {

    val command = new InputParser(input).prepareResultObject match {
      case input: ReactFunction => react(input)
      case input: WelcomeFunction => welcome(input)
      case input: GoodbyeFunction => goodbye(input)
    }

    println(command)

    command
  }


  def react(input: ReactFunction): String = {

    val userName = "Marcin"


    "Move(direction="+randomMove+":"+randomMove+")"
  }





  def randomMove: Int = {
    val randomValue: Double = math.random

    if (randomValue > 0.66) {
      1
    } else if (randomValue > 0.33) {
      0
    } else {
      -1
    }
  }

  def welcome(input: WelcomeFunction): String = {
    "Status(text=Preparing)"
  }

  def goodbye(input: GoodbyeFunction): String = {
    "Status(text=Going out)"
  }
}