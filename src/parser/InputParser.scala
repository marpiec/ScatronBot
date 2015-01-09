package parser

import data.XY
import function._

class InputParser(input: String) {

  private val parsed:ParsedInput = RawParser.parse(input)

  val functionName = parsed.functionName
  val params = parsed.params

  val result = prepareResultObject

  def prepareResultObject: AnyRef = {
    if (functionName == "React") {
      new ReactFunction(
        params.getOrElse("generation", "0").toInt,
        params.getOrElse("name", ""),
        params.getOrElse("time", "0").toInt,
        params.getOrElse("view", ""),
        params.getOrElse("energy", "0").toInt,
        getXYParamOption("master"),
        getXYParamOption("collision"),
        params.getOrElse("slaves", "0").toInt)
    } else if (functionName == "Welcome") {
      new WelcomeFunction(params.getOrElse("name", ""),
        params.getOrElse("apocalypse", "0").toInt,
        params.getOrElse("round", "0").toInt,
        params.getOrElse("maxSlaves", "0").toInt)
    } else if (functionName == "Goodbye") {
      new GoodbyeFunction(params.getOrElse("energy", "0").toInt)
    } else {
      throw new IllegalArgumentException("Unknown method received")
    }
  }

  def getXYParamOption(name: String): Option[XY] = {
    val paramOption = params.get(name)
    if (paramOption.isDefined) {
      Option(XY.parse(paramOption.get))
    } else {
      None
    }
  }

}
