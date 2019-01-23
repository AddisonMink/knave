package knave.world.dungeon

import knave.display.Palette._
import knave.world.dungeon.Size.{height, width}

import scala.collection.mutable.ListBuffer
import scala.util.Random

package object Size {
  val height = 20
  val width = 80
}

abstract class Dungeon {

  final protected val bloodColor = crimson

  final protected  val darkBloodColor = darkRed

  protected val tileArray = Array.ofDim[Tile](width,height)

  final def isFloor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

  final def floorAt(c: Coord): Option[Floor] = tileArray(c.x)(c.y) match {
    case f : InnerFloor => Some(Floor(f.color, f.darkColor, f.symbol))
    case _ => None
  }

  final def isWall(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerWall]

  final def wallAt(c: Coord): Option[Wall] = tileArray(c.x)(c.y) match {
    case w : InnerWall => Some(Wall(w.color, w.darkColor, w.symbol))
    case _ => None
  }

  final def isDoor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerDoor]

  final def doorAt(c: Coord): Option[Door] = tileArray(c.x)(c.y) match {
    case d : InnerDoor => Some(Door(d.color, d.darkColor, d.open, d.symbol))
    case _ => None
  }

  final def openDoor(c: Coord): Unit = tileArray(c.x)(c.y) match {
    case d : InnerDoor => d.open = true
    case _ => ()
  }

  final def isStairs(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[Stairs]

  final def bloodyTile(c: Coord): Unit = {
    val tile = tileArray(c.x)(c.y)
    tile.color = bloodColor
    tile.darkColor = darkBloodColor
  }

  final def createCorpse(c: Coord): Unit = {
    if(tileArray(c.x)(c.y).isInstanceOf[InnerFloor])
      tileArray(c.x)(c.y) = new Corpse(bloodColor, darkBloodColor)
  }

  private val visitedCoords = collection.mutable.Set[Coord]()

  final def visited : Set[Coord] =
    visitedCoords.toSet

  final def visitCoords(cs : Iterable[Coord]) : Unit =
    visitedCoords ++= cs

  final def isWalkable(c : Coord) : Boolean =
    isFloor(c) || doorAt(c).map(_.open).getOrElse(false)

  final def visibleLine(start : Coord, end : Coord) : Stream[Coord] = {
    var barrierEncountered = true
    start.lineTo(end).takeWhile(c => {
      if(!isWalkable(c)) {
        if(barrierEncountered) {
          barrierEncountered = false
          true
        }
        else false
      }
      else barrierEncountered
    })
  }

  final def walkableLine(start : Coord, end : Coord) : Stream[Coord] =
    start.lineTo(end).takeWhile(isWalkable(_))

  final def fieldOfVision(center : Coord, radius : Int) : Set[Coord] = {
    val cs = new ListBuffer[Coord]
    var i = radius
    do {
      val rim = ((center.x - i) to (center.x + i)).toStream
        .flatMap(tempX => {
          val dx = Math.abs(center.x - tempX)
          val dy = i - dx
          Stream(Coord(tempX, center.y + dy), Coord(tempX, center.y - dy))
        })
      cs ++= rim.flatMap(visibleLine(center,_)).toList
      i -= 1
    } while(i > 3)
    cs.toSet
  }

  final def castRay(start : Coord, end : Coord) : Boolean =
    visibleLine(start,end).contains(end)

  final def castRay(start : Coord, end : Coord, limit : Int) : Boolean =
    visibleLine(start,end).take(limit).contains(end)

  final def findPath(start : Coord, end : Coord) : List[Coord] = {

    def loop(paths : List[List[Coord]], visited : Set[Coord]) : List[Coord] =
      if(paths.isEmpty || paths.head.isEmpty) List()
      else {
        val path = paths.head
        if(path.head == end) path
        else {
          val moves = path.head.adjacent.filter(c => !visited.contains(c) && isWalkable(c)).sortBy(c => c.distance(end) + c.manhattanDistance(path.head)).toList
          val newVisited = visited.union(moves.toSet)
          val newPaths = moves.map(_ :: path) ++ paths.tail
          loop(newPaths, newVisited)
        }
      }

    if(start == end) List()
    else {
      val path = loop(List(List(start)), Set(start))
      if(path.isEmpty) List()
      else path.reverse.tail
    }
  }

  def rooms : List[Room]
}

object Dungeon {

  def createHubDungeon(seed : Int) : Dungeon = new HubDungeon(seed)

  def createRandomRoomsDungeon(seed : Int) : Dungeon = new RandomRoomsDungeon(seed)

  def createOpenDungeon : Dungeon = new OpenDungeon
}
