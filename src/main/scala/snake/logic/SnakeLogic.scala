package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game._
import snake.logic.SnakeLogic._

import scala.collection.mutable.ListBuffer

/** To implement Snake, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``snake`` package.
  */
class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  private val gameMap = Array.fill[GridType](nrRows, nrColumns)(Empty())
  def updateGameMap() {
    for (cell <- snakeCells) {
      gameMap(cell(0))(cell(1)) = SnakeBody() //Implement the colour here
    }
    gameMap(snakeCells.head(0))(snakeCells.head(1)) = SnakeHead(direction)
  }

  private var direction: Direction = East()
  private var previousDirection = direction
  private var snakeCells: ListBuffer[ListBuffer[Int]] = ListBuffer(
    ListBuffer(0,2),
    ListBuffer(0,1),
    ListBuffer(0,0)
  )

  private var freeCells: ListBuffer[ListBuffer[Int]] = ListBuffer()
  for (row <- 0 until nrRows) {
    for (col <- 0 until nrColumns)
      freeCells += ListBuffer(row,col)
    }
  for (cell <- snakeCells)
    freeCells-= cell

  var applePlaced = false
  placeApple()
  updateGameMap()

  def this() {
    this(new ScalaRandomGen(), DefaultColumns, DefaultRows)
  }

  def isGameOver: Boolean = false

  def step(): Unit = {
    getDirection()
    previousDirection = direction
    moveSnake()
    freeCells = ListBuffer()
    for (row <- 0 until nrRows) {
      for (col <- 0 until nrColumns)
        freeCells += ListBuffer(row,col)
      }
    for (cell <- snakeCells)
      freeCells-= cell
    println(direction)
    println(previousDirection)
    println("===========")
    placeApple()
    updateGameMap()
  }

  def setReverseTime(reverse: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = gameMap(snakeCells.head(0))(snakeCells.head(1)) = SnakeHead(d)

  def getGridTypeAt(x: Int, y: Int): GridType = gameMap(y)(x)

  def moveSnake(): Unit = {
    snakeCells.prepend(ListBuffer(snakeCells.head(0), snakeCells.head(1)))
     direction match {
        case West()  =>  snakeCells.head(1) = (snakeCells(1)(1)-1+nrColumns)%nrColumns
        case North()  => snakeCells.head(0) = (snakeCells(1)(0)-1+nrRows)%nrRows
        case East() =>   snakeCells.head(1) = (snakeCells(1)(1)+1)%nrColumns
        case South()  => snakeCells.head(0) = (snakeCells(1)(0)+1)%nrRows
        case _ => println("Error: Snake head not found")
     }
    if (gameMap(snakeCells.head(0))(snakeCells.head(1)) == Apple()) eatApple()
    else gameMap(snakeCells.last(0))(snakeCells.last(1)) = Empty()
    snakeCells = snakeCells.dropRight(1);
  }

  def getDirection(): Unit = {
    gameMap(snakeCells.head(0))(snakeCells.head(1)) match {
      case SnakeHead(West())  => if (directionAllowed(direction)) direction = West()
      case SnakeHead(North())  => if (directionAllowed(direction)) direction = North()
      case SnakeHead(East())  => if (directionAllowed(direction)) direction = East()
      case SnakeHead(South())  => if (directionAllowed(direction)) direction = South()
      case _ => println("Error: Snake head not found")
    }
  }

 def directionAllowed(triedDirection:Direction):Boolean = {
   var allowed = true
//   triedDirection match {
//     case West() => allowed = previousDirection != West()
//     case North() => allowed = previousDirection != North()
//     case East() => allowed = previousDirection != East()
//     case South() => allowed = previousDirection != South()
//   }
   return allowed
 }

  def placeApple(): Unit = {
    val freeCellsSize:Int = freeCells.size
    if (freeCellsSize == 0) return
    val appleCell:Int = randomGen.randomInt(freeCellsSize)
    val appleRow = freeCells(appleCell)(0)
    val appleCol = freeCells(appleCell)(1)

    if (!applePlaced) gameMap(appleRow)(appleCol) = Apple()
    applePlaced = true
  }

  def eatApple(): Unit = {
    snakeCells.append(ListBuffer(snakeCells.last(0), snakeCells.last(1)))
    applePlaced = false
  }
}

/** SnakeLogic companion object */
object SnakeLogic {

  val DefaultColumns = 5
  val DefaultRows = 2

}
