import scala.annotation.switch
import scala.collection
import scala.collection.parallel.mutable

//** DATA ******************************************************************

object EntitiesTypes {

  final val INVISIBLE = '?'
  final val EMPTY = '_'
  final val WALL = 'W'
  final val MY_BOT = 'M'
  final val ENEMY_BOT = 'm'
  final val MY_MINI_BOT = 'S'
  final val ENEMY_MINI_BOT = 's'
  final val GOOD_PLANT = 'P'
  final val BAD_PLANT = 'p'
  final val GOOD_BEAST = 'B'
  final val BAD_BEAST = 'b'

  def isInvisible(pointValue: Char) = INVISIBLE == pointValue
  def isEmpty(pointValue: Char) = EMPTY == pointValue
  def isWall(pointValue: Char) = WALL == pointValue
  def isMyBot(pointValue: Char) = MY_BOT == pointValue
  def isEnemyBot(pointValue: Char) = ENEMY_BOT == pointValue
  def isMyMiniBot(pointValue: Char) = MY_MINI_BOT == pointValue
  def isEnemyMiniBot(pointValue: Char) = ENEMY_MINI_BOT == pointValue
  def isGoodPlant(pointValue: Char) = GOOD_PLANT == pointValue
  def isBadPlant(pointValue: Char) = BAD_PLANT == pointValue
  def isGoodBeast(pointValue: Char) = GOOD_BEAST == pointValue
  def isBadBeast(pointValue: Char) = BAD_BEAST == pointValue

  def isSafeEntity(pointValue: Char) = !notSafeEntity(pointValue)
  def notSafeEntity(pointValue: Char) = isWall(pointValue) || isBadBeast(pointValue) ||
    isBadPlant(pointValue) || isEnemyBot(pointValue) || isEnemyMiniBot(pointValue)
}

case class Direction(x: Double, y: Double) {
  def +(other: Direction) = {
    Direction(x + other.x, y + other.y)
  }
  def -(other: Direction) = {
    Direction(x - other.x, y - other.y)
  }
  def *(scale: Double) = {
    Direction(x * scale, y * scale)
  }
  def /(scale: Double) = {
    Direction(x / scale, y / scale)
  }
  def normalize = this / distance
  def distance = math.sqrt(x*x + y*y)
  def toOne = {
    val tan = math.abs(x / y)
    val newX = if(tan > Direction.s225) {
      if(x < 0) -1 else 1
    } else {
      0
    }
    val newY = if(tan < Direction.s667) {
      if(y < 0) -1 else 1
    } else {
      0
    }

    XY(newX, newY)
  }
  override def toString = x + ":" + y
}

object Direction {
  final val s225 = 0.41421356237 // tan 22.5 degrees
  final val s667 = 2.41421356237 // tan 67.5 degrees
  val ZERO = Direction(0, 0)
}


case class NamedDirection(direction: Direction, name: String)

case class XY(x: Int, y: Int) {
  def +(other: XY) = {
    XY(x + other.x, y + other.y)
  }
  def -(other: XY) = {
    XY(x - other.x, y - other.y)
  }
  def *(scale: Int) = {
    XY(x * scale, y * scale)
  }
  def distance = toDirection.distance
  def toDirection = Direction(x.toDouble, y.toDouble)

  override def toString = x + ":" + y
}

object XY {
  val ZERO = XY(0, 0)
  def apply(input: String) = {
    val coords = input.split(":")
    new XY(coords(0).toInt, coords(1).toInt)
  }
}


//** INPUT PARSER **********************************************************

case class ParsedInput(functionName: String, params: Map[String, String])

class InputIterator(input: String) {
  val inputLength = input.length
  var index = 0
  def next = {
    val ch = input.charAt(index)
    index += 1
    ch
  }
  def hasNext = index < inputLength
}


class InputParser(input: String) {

  private val parsed:ParsedInput = RawParser.parse(input)

  val functionName = parsed.functionName
  val params = parsed.params

  val result = prepareResultObject

  def prepareResultObject: AnyRef = {
    if (functionName.charAt(0) == 'R') { //React
      new ReactFunction(
        params.getOrElse("generation", "0").toInt,
        params.getOrElse("name", ""),
        params.getOrElse("time", "0").toInt,
        params.getOrElse("view", ""),
        params.getOrElse("energy", "0").toInt,
        getXYParamOption("master"),
        getXYParamOption("collision"),
        params.getOrElse("slaves", "0").toInt,
        params.getOrElse("timeFromCreation", "0").toInt)
    } else if (functionName.charAt(0) == 'W') { //Welcome
      new WelcomeFunction(params.getOrElse("name", ""),
        params.getOrElse("apocalypse", "0").toInt,
        params.getOrElse("round", "0").toInt,
        params.getOrElse("maxSlaves", "0").toInt)
    } else if (functionName.charAt(0) == 'G') { //Goodbye
      new GoodbyeFunction(params.getOrElse("energy", "0").toInt)
    } else {
      throw new IllegalArgumentException("Unknown method received " + functionName)
    }
  }

  def getXYParamOption(name: String): Option[XY] = {
    val paramOption = params.get(name)
    if (paramOption.isDefined) {
      Option(XY(paramOption.get))
    } else {
      None
    }
  }

}




object RawParser {

  def parse(input: String): ParsedInput = {

    val iterator = new InputIterator(input)
    val functionName = readUntil(iterator, _ != '(')

    var params = collection.mutable.ListMap[String, String]()

    while(iterator.hasNext) {
      val name = readUntil(iterator, _ != '=')
      val value = readUntil(iterator, (ch) => ch != ',' && ch != ')')
      params += name -> value
    }

    ParsedInput(functionName, params.toMap)
  }

  def readUntil(iterator: InputIterator, predicate: Char => Boolean): String = {
    val sb = new StringBuilder

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



//** VIEW ANALYSER ********************************************************

class ViewAnalyser(view: String) {


  val viewLength = view.length
  val viewSize = math.sqrt(view.length).toInt
  val viewDistance = (viewSize - 1) / 2

  var goodPlants = List[XY]()
  var goodBeasts = List[XY]()
  var badPlants = List[XY]()
  var badBeasts = List[XY]()
  var enemyBots = List[XY]()
  var enemyMiniBots = List[XY]()
  var myBots = List[XY]()
  var myMiniBots = List[XY]()
  var walls = List[XY]()

  //init lists
  findEntities()

  def getViewPoint(x: Int, y: Int): Char = view.charAt(y * viewSize + x)

  def enemiesCount = badPlants.size + enemyBots.size + enemyMiniBots.size

  private def findEntities() {
    var i = 0
    var x = -viewDistance
    var y = -viewDistance
    while (i<viewLength) {
      addEntityToProperList(view.charAt(i), x, y)

      i += 1
      if(x < viewDistance) {
        x += 1
      } else {
        x = -viewDistance
        y += 1
      }

    }
  }

  private def addEntityToProperList(viewPoint: Char, x: Int, y: Int) {
    (viewPoint: @switch) match {
      case EntitiesTypes.GOOD_PLANT => goodPlants ::= XY(x, y)
      case EntitiesTypes.GOOD_BEAST => goodBeasts ::= XY(x, y)
      case EntitiesTypes.BAD_PLANT => badPlants ::= XY(x, y)
      case EntitiesTypes.BAD_BEAST => badBeasts ::= XY(x, y)
      case EntitiesTypes.ENEMY_BOT => enemyBots ::= XY(x, y)
      case EntitiesTypes.ENEMY_MINI_BOT => enemyMiniBots ::= XY(x, y)
      case EntitiesTypes.MY_BOT => myBots ::= XY(x, y)
      case EntitiesTypes.MY_MINI_BOT => myMiniBots ::= XY(x, y)
      case EntitiesTypes.WALL => walls ::= XY(x,y)
      case _ => ()
    }
  }

}



//** FUNCTIONS ************************************************************

case class GoodbyeFunction(energy: Int)

case class WelcomeFunction(name: String, apocalypse: Int, round: Int, maxSlaves: Int)

case class ReactFunction(generation: Int, name: String, time: Int, view: String,
                    energy: Int, masterOption: Option[XY], collisionOption: Option[XY],
                    slaves: Int, timeFromCreation: Int) {


  val viewDistance = calculateViewDistance


  def calculateViewDistance: Double = {
    if (view.isEmpty) {
      0
    }
    else {
      (math.sqrt(view.length) - 1) / 2
    }
  }

  def viewToFormattedString() = {
    val sb = new StringBuilder()
    val viewSize = math.sqrt(view.length).toInt
    for (y <- 0 until viewSize) {
      for (x <- 0 until viewSize) {
        sb.append(view.charAt(x + y * viewSize)).append(" ")
      }
      sb.append("\n")
    }
    sb.toString()
  }
}


//** Commands ********************************************************


trait Command

case class Explode(val size: Int) extends Command {
  override def toString = {
    "Explode(size=" + size.toString + ")"
  }
}


case class Move(val direction: XY) extends Command {
  override def toString = {
    "Move(direction=" + direction.toString + ")"
  }
}

case class Spawn(var direction: XY, energy: Int) extends Command {
  override def toString: String = {
    val sb = new StringBuilder("Spawn(direction=").append(direction.toString).
      append(",energy=").append(energy.toString)

    sb.append(")")
    sb.toString()
  }
}

case class SetCommand(params: Map[String, String]) extends Command {
  override def toString: String = {

    val sb = new StringBuilder("Set(")

    var notFirst = false
    for ((key, value) <- params) {
      if (notFirst) {
        sb.append(",")
      } else {
        notFirst = true
      }
      sb.append(key).append("=").append(value)
    }

    sb.append(")").toString()
  }
}


class Commands(val commands: List[Command]) {

  def this(command: Command) = this(List(command))

  def ::(command: Command): Commands = {
    new Commands(command :: commands)
  }

  def :::(otherCommands: Commands): Commands = {
    new Commands(otherCommands.commands ::: commands)
  }

  def isEmpty = commands.isEmpty

  override def toString: String = {
    val sb = new StringBuilder()
    var nonFirst = false
    for (command <- commands) {
      if (nonFirst) {
        sb.append("|")
      } else {
        nonFirst = true
      }
      sb.append(command)
    }
    sb.toString()
  }
}




//** MAIN ************************************************************

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {

  var apocalypseAt: Int = 0;

  def respond(input: String):String = {

    try {
      val command = new InputParser(input).prepareResultObject match {
        case input: ReactFunction => if(input.generation==0) reactMaster(input) else reactMini(input)
        case input: WelcomeFunction => welcome(input)
        case input: GoodbyeFunction => goodbye(input)
      }

     // println(command)

      command
    }catch {
      case e:Exception => e.printStackTrace(); throw e;
    }
  }



  def reactMini(input: ReactFunction): String = {


    val analyser = new ViewAnalyser(input.view)

    var instinct = List[NamedDirection]()

    instinct ::= NamedDirection(analyser.goodPlants.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.0
      val nutritions = 1.5
      acc + entity.toDirection.normalize * nutritions / pathCost
    }), "plantHunger")

    instinct ::= NamedDirection(analyser.goodBeasts.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.5
      val nutritions = 3
      acc + entity.toDirection.normalize * nutritions / pathCost
    }), "beastHunger")

    instinct ::= NamedDirection(analyser.walls.foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance / 10
    }), "wall")

    instinct ::= NamedDirection(analyser.walls.filter(_.distance < 2).foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance * entity.distance * 2
    }), "wall")

    instinct ::= NamedDirection(analyser.badPlants.filter(_.distance < 2).foldLeft(Direction.ZERO)((acc, entity) => {
      acc + entity.toDirection.normalize * 2
    }), "plantFear")

    instinct ::= NamedDirection(analyser.badBeasts.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.5
      val nutritions = 0.3
      acc + entity.toDirection.normalize * nutritions / pathCost
    }), "beastFear")

    instinct ::= NamedDirection(analyser.myMiniBots.filterNot(_.distance == 0).foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance / entity.distance * 25 * math.max(0, 100 - input.timeFromCreation) / 100
    }), "loner")

    instinct ::= NamedDirection(analyser.enemyMiniBots.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.5
      val nutritions = 1
      acc + entity.toDirection.normalize * nutritions / pathCost
    }), "predator")

    instinct ::= NamedDirection(analyser.enemyBots.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.2
      val nutritions = 8
      acc + entity.toDirection.normalize * nutritions / pathCost
    }), "Predator")

//    val timePhase = input.timeFromCreation / 50 % 4
//
//    instinct ::= NamedDirection((timePhase match {
//      case 0 => Direction(-1, 1)
//      case 1 => Direction(-1, -1)
//      case 2 => Direction(1, -1)
//      case 3 => Direction(1, 1)
//    }) / 10, "wander")

    if(input.timeFromCreation > 20) {

      val master = input.masterOption.get.toDirection.normalize
      val masterDistance = master.distance
      instinct ::= NamedDirection(master / 5 * math.pow(input.energy.toDouble / 200.0, 1.5) / masterDistance * (if(masterDistance < 50) 1.0 else 2.0), "GoHome")

    }

    if(input.time > apocalypseAt - 100) {
      instinct ::= NamedDirection(input.masterOption.get.toDirection.normalize * 100, "AbandonShip")
    }

    // else {
      val master = input.masterOption.get.toDirection.normalize
      val masterDistance = master.distance
      instinct ::= NamedDirection(master * (-0.05) * math.max(0.0, 100.0 - masterDistance) / 100, "master")
//
//    }

    // instinct ::= Direction(-0.3, 0)


    val moveTo = instinct.foldLeft(Direction.ZERO){(acc, entity) => acc + entity.direction}
//println(instinct)
    //println("goodPlant " + goodPlant + "goodBeast " + goodBeast + "badPlant " + badPlant + "badBeast " + badBeast + "moveTo "+ moveTo + "one " + moveTo.toOne)


    var commands = new Commands(new Move(moveTo.toOne))

    val enemies = analyser.badBeasts ::: analyser.enemyMiniBots ::: analyser.enemyBots
      if(enemies.count(_.distance < 3) > 1) {
        commands ::= Explode(3)
      } else if (enemies.count(_.distance < 2) > 0) {
        commands ::= Explode(2)
      }
//      else if(analyser.badPlants.size > 0 && analyser.badPlants.map(_.distance).min < 2) {
//      commands ::= Explode(2)
//    }



    if(input.energy > 140 && analyser.myMiniBots.length < 3 && analyser.myMiniBots.filter(_.distance < 5).size < 2) {
      commands ::= new Spawn(moveTo.toOne * -1, 100)
    }
    commands ::= SetCommand(Map("timeFromCreation" -> (input.timeFromCreation + 1).toString))

    commands.toString
  }

  def reactMaster(input: ReactFunction): String = {


    val analyser = new ViewAnalyser(input.view)

    var instinct = List[Direction]()

    instinct ::= analyser.goodPlants.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.0
      val nutritions = 1
      acc + entity.toDirection.normalize * nutritions / pathCost * 2
    })

    instinct ::= analyser.goodBeasts.foldLeft(Direction.ZERO)((acc, entity) => {
      val pathCost = entity.distance * 1.5
      val nutritions = 2
      acc + entity.toDirection.normalize * nutritions / pathCost * 2
    })

    instinct ::= analyser.walls.foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance
    })

    instinct ::= analyser.badPlants.filter(_.distance < 2).foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / (entity.distance * entity.distance)
    })

    instinct ::= analyser.badBeasts.foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance * 2
    })

    instinct ::= analyser.enemyMiniBots.foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize / entity.distance * 4
    })

    instinct ::= analyser.myMiniBots.filterNot(_.distance == 0).foldLeft(Direction.ZERO)((acc, entity) => {
      acc - entity.toDirection.normalize * entity.distance / 10
    })


    val timePhase = input.time / 200 % 4

    instinct ::= (timePhase match {
      case 0 => Direction(-1, 1)
      case 1 => Direction(-1, -1)
      case 2 => Direction(1, -1)
      case 3 => Direction(1, 1)
    }) / 2


    val moveTo = instinct.reduceLeft((acc, entity) => acc + entity)

    var commands = new Commands(new Move(moveTo.toOne))

    if(input.energy > 200 && analyser.myMiniBots.length < 4) {
      commands ::= new Spawn(moveTo.toOne * -1, if(input.energy > 500) 200 else 100)
    }
    commands ::= SetCommand(Map("timeFromCreation" -> (input.timeFromCreation + 1).toString))

    commands.toString

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
    apocalypseAt = input.apocalypse
    "Status(text=Preparing)"
  }

  def goodbye(input: GoodbyeFunction): String = {
    "Status(text=Going out)"
  }
}