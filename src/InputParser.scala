import data.XY
import function._

class InputParser(input: String) {

  private val (parsedFunctionName, parsedParams) = parse(input)

  val functionName = parsedFunctionName

  val result = prepareResultObject

  def prepareResultObject: AnyRef = {
    if (functionName == "React") {
      new ReactFunction(
        parsedParams.getOrElse("generation", "0").toInt,
        parsedParams.getOrElse("name", ""),
        parsedParams.getOrElse("time", "0").toInt,
        parsedParams.getOrElse("view", ""),
        parsedParams.getOrElse("energy", "0").toInt,
        getXYParamOption("master"),
        getXYParamOption("collision"),
        parsedParams.getOrElse("slaves", "0").toInt)
    } else if (functionName == "Welcome") {
      new WelcomeFunction(parsedParams.getOrElse("name", ""),
        parsedParams.getOrElse("apocalypse", "0").toInt,
        parsedParams.getOrElse("round", "0").toInt,
        parsedParams.getOrElse("maxSlaves", "0").toInt)
    } else if (functionName == "Goodbye") {
      new GoodbyeFunction(parsedParams.getOrElse("energy", "0").toInt)
    } else {
      throw new IllegalArgumentException("Unknown method received")
    }
  }

  def getXYParamOption(name: String): Option[XY] = {
    val paramOption = parsedParams.get(name)
    if (paramOption.isDefined) {
      Option(XY.parse(paramOption.get))
    } else {
      None
    }
  }

  /** Got from https://github.com/scalatron/scalatron/blob/master/Scalatron/doc/markdown/Scalatron%20Tutorial.md#bot-5-creating-a-command-parser-function */
  def parse(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if (segments.length != 2)
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0), segments(1))
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)

    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParam).toMap
    (segments(0), keyValuePairs)
  }
}
