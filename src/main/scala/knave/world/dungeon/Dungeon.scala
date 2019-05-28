package knave.world.dungeon

import knave.display.Palette._
import knave.world.dungeon.Size.{height, width}

import scala.collection.immutable.HashSet.HashSet1
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

// TODO This should be part of Dungeon.
package object Size {
  val height = 20
  val width = 80
}

abstract class Dungeon(seed : Int) {

  final val rng = new Random(seed)

  // TODO This should be in Palette.
  final protected val bloodColor = crimson

  // TODO This should be in Palette.
  final protected  val darkBloodColor = darkRed

  protected val tileArray = Array.ofDim[InnerTile](width,height)

  // TODO For all the is_ and _at, methods, make is_ = _at.nonEmpty
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

  final def createStairs(c : Coord) : Unit = tileArray(c.x)(c.y) = new Stairs(lightGray,darkGray)

  final def bloodyTile(c: Coord): Unit = {
    val tile = tileArray(c.x)(c.y)
    tile.color = bloodColor
    tile.darkColor = darkBloodColor
  }

  final def createCorpse(c: Coord): Unit = {
    if(tileArray(c.x)(c.y).isInstanceOf[InnerFloor] && !isStairs(c))
      tileArray(c.x)(c.y) = new Corpse(bloodColor, darkBloodColor)
  }

  // TODO There is no reason to expose only a frozen version of the set if you are exposing methods to to mutate the set.
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

  // TODO Maybe create an implicit class so these methods can be called on a coord in the context of a dungeon.
  final def walkableLine(start : Coord, end : Coord) : Stream[Coord] =
    start.lineTo(end).takeWhile(isWalkable(_))

  final def nextWalkableCoord(start : Coord, end : Coord) : Option[Coord] =
    walkableLine(start,end).headOption

  // TODO This can probably be made cleaner.
  // TODO There is no reason to expose only a frozen version of the set if you are exposing methods to to mutate the set.
  final def circle(center : Coord, radius : Int) : Set[Coord] = {
    val cs = new ListBuffer[Coord]
    cs += center
    var i = radius
    do {
      val rim = ((center.x - i) to (center.x + i)).toStream
        .flatMap(tempX => {
          val dx = Math.abs(center.x - tempX)
          val dy = i - dx
          Stream(Coord(tempX, center.y + dy), Coord(tempX, center.y - dy))
        })
      cs ++= rim.flatMap(walkableLine(center,_)).toList
      i -= 1
    } while(i > 3)
    cs.toSet
  }

  // TODO This can probably be made cleaner.
  // TODO There is no reason to expose only a frozen version of the set if you are exposing methods to to mutate the set.
  final def fieldOfVision(center : Coord, radius : Int) : Set[Coord] = {
    val cs = new ListBuffer[Coord]
    cs += center
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

  // TODO This can probably be made cleaner.
  // TODO Maybe move this to an implicit class so it can be called on Coord in the context of a dungeon.
  final def cone(center : Coord, direction : Coord, vision : Int) : Set[Coord] = {
    val dir = direction.normalize


    lazy val straight : Set[Coord] = {
      val fov = new ListBuffer[Coord]
      fov += center
      val periphery = Seq(Coord(center.x + dir.y, center.y + dir.x), Coord(center.x - dir.y, center.y - dir.x))
      for(v <- 1 to vision) {
        val d = v / 2 + 1
        val top = if(dir.x != 0) Coord(center.x + v*dir.x, center.y - d) else Coord(center.x - d, center.y + v*dir.y)
        val bottom = if(dir.x != 0) Coord(center.x + v*dir.x, center.y + d) else Coord(center.x + d, center.y + v*dir.y)
        val rim = top #:: top.lineTo(bottom)
        fov ++= rim.flatMap(walkableLine(center,_))
      }
      fov ++= periphery
      fov.toSet
    }

    lazy val diagonal : Set[Coord] = {
      val fov = new ListBuffer[Coord]
      fov += center
      val periphery = Seq(Coord(center.x, center.y + dir.y), Coord(center.x + dir.x, center.y))
      fov ++= periphery
      for(v <- 1 to vision) {
        val right = Coord(center.x + v*dir.x, center.y)
        val down = Coord(center.x, center.y + v*dir.y)
        val diag = Coord(center.x + v*dir.x, center.y + v*dir.y)
        val rim = right #:: right.lineTo(diag) ++ diag.lineTo(down)
        fov ++= rim.flatMap(walkableLine(center,_))
      }
      fov.toSet
    }

    if(dir.x == 0 && dir.y == 0) Set()
    else if(dir.x != 0 && dir.y != 0) diagonal
    else straight
  }



  // TODO Same goes for these methods.
  final def castRay(start : Coord, end : Coord) : Boolean =
    visibleLine(start,end).contains(end)

  final def castRay(start : Coord, end : Coord, limit : Int) : Boolean =
    visibleLine(start,end).take(limit).contains(end)

  final def findPath(start : Coord, end : Coord, openDoors : Boolean = false) : List[Coord] = {
    val paths = new ListBuffer[List[Coord]] ; paths += List(start)
    val visited = new mutable.HashSet[Coord] ; visited += start
    var result = List[Coord]()

    while(paths.nonEmpty && result.isEmpty) {
      val path = paths.remove(0)
      if(path.head == end)
        result = path
      else {
        val validMove: Coord => Boolean =
          if(openDoors) (c : Coord) => !visited.contains(c) && (isWalkable(c) || isDoor(c))
          else (c : Coord) => !visited.contains(c) && isWalkable(c)
        val moves = path.head.adjacent.filter(validMove).sortBy(_.manhattanDistance(path.head))
        visited ++= moves
        val newPaths = moves.map(_ :: path)
        paths ++= newPaths
      }
    }
    result.reverse.tail
  }

  def rooms : List[Room]
}

// TODO Get rid of this. It doesn't really help with encapsulation.
object Dungeon {

  def hubDungeon(seed : Int) : Dungeon = new HubDungeon(seed)

  def openDungeon(seed : Int) : Dungeon = new OpenDungeon(seed)
}
