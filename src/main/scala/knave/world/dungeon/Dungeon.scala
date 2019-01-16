package knave.world.dungeon

package object Size {
  val height = 20
  val width = 78
}

trait Dungeon {

  final protected val bloodColor = "#DC143C"

  final protected  val darkBloodColor = "#8B0000"

  def isFloor(c : Coord) : Boolean
  def floorAt(c : Coord) : Option[Floor]

  def isWall(c : Coord) : Boolean
  def wallAt(c : Coord) : Option[Wall]

  def isDoor(c : Coord) : Boolean
  def doorAt(c : Coord) : Option[Door]
  def openDoor(c : Coord) : Unit

  def bloodyTile(c : Coord) : Unit

  def createCorpse(c : Coord) : Unit

  def isStairs(c : Coord) : Boolean

  private val visitedCoords = collection.mutable.Set[Coord]()

  final def visited : Set[Coord] =
    visitedCoords.toSet

  final def visitCoords(cs : Iterable[Coord]) : Unit =
    visitedCoords ++= cs

  final def isWalkable(c : Coord) : Boolean =
    isFloor(c) || doorAt(c).map(_.open).getOrElse(false)

  final def visibleLine(start : Coord, end : Coord) : Stream[Coord] = {
    var valid = true
    start.lineTo(end).takeWhile(c => {
      if(!isWalkable(c)) {
        if(valid) {
          valid = false
          true
        }
        else false
      }
      else valid
    })
  }

  final def fieldOfVision(center : Coord, radius : Int) : Set[Coord] = {
    val rim = ((center.x - radius) to (center.x + radius)).toStream
      .flatMap(tempX => {
        val dx = Math.abs(center.x - tempX)
        val dy = radius - dx
        Stream(Coord(tempX, center.y + dy), Coord(tempX, center.y - dy))
      })
    rim.flatMap(visibleLine(center,_)).toSet
  }

  final def castRay(start : Coord, end : Coord) : Boolean =
    visibleLine(start,end).contains(end)

  final def castRay(start : Coord, end : Coord, limit : Int) : Boolean =
    visibleLine(start,end).take(limit).contains(end)

  def rooms : List[Room]
}

object Dungeon {

  def createRandomRoomsDungeon(seed : Int) : Dungeon = new RandomRoomsDungeon(seed)
}
