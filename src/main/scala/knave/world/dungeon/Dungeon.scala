package knave.world.dungeon

import knave.display.Palette.{bloodColor, darkBloodColor, darkGray, lightGray}
import knave.world.dungeon.Room.RoomGraph

import scala.annotation.tailrec
import scala.util.Random

sealed trait Dungeon {
  require(tileArray.length == Dungeon.width && tileArray(0).length == Dungeon.height)

  implicit val rng: Random

  val rooms: Seq[Room]

  val graph: RoomGraph

  protected val tileArray: Array[Array[InnerTile]]

  final def floorAt(c: Coord): Option[Floor] = tileArray(c.x)(c.y) match {
    case f : InnerFloor => Some(f.tile)
    case _ => None
  }
  final def isFloor(c: Coord): Boolean = floorAt(c).nonEmpty

  final def wallAt(c: Coord): Option[Wall] = tileArray(c.x)(c.y) match {
    case w : InnerWall => Some(w.tile)
    case _ => None
  }
  final def isWall(c: Coord): Boolean = wallAt(c).nonEmpty

  final def doorAt(c: Coord): Option[Door] = tileArray(c.x)(c.y) match {
    case d : InnerDoor => Some(d.tile)
    case _ => None
  }
  final def isDoor(c: Coord): Boolean = doorAt(c).nonEmpty

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
}

protected class InnerDungeon(val tileArray: Array[Array[InnerTile]], val rooms: Seq[Room], val graph: RoomGraph, implicit val rng: Random) extends Dungeon

object Dungeon {
  val height = 20

  val width = 80

  def apply(seed: Int): Dungeon = HubDungeon(seed)

  implicit class DungeonCoord(c: Coord) {

    def isWalkable(implicit dungeon: Dungeon): Boolean =
      c.inBounds && (dungeon.isFloor(c) || dungeon.doorAt(c).exists(_.open))

    def walkableLineTo(c2: Coord)(implicit dungeon: Dungeon): Seq[Coord] =
      c.lineTo(c2).takeWhile(_.isWalkable)

    def nextWalkable(c2: Coord)(implicit dungeon: Dungeon): Option[Coord] =
      c.toward(c2).filter(_.isWalkable)

    def hasWalkableLineTo(c2: Coord, maxDistance: Int = Int.MaxValue)(implicit dungeon: Dungeon): Boolean =
      c.walkableLineTo(c2).take(maxDistance).contains(c2)

    def walkableCone(radius: Int, dir: Direction)(implicit dungeon: Dungeon): Seq[Coord] =
      c.cone(radius, dir, _.isWalkable)

    def enemyConeOfVision(radius: Int, dir: Direction)(implicit dungeon: Dungeon): Seq[Coord] = {
      val peripheral = dir match {
        case Horizontal(dx) => Seq(c + (0,1), c + (0,-1), c + (dx,1), c + (dx,-1)).filter(_.isWalkable)
        case Vertical(dy) => Seq(c + (1,0), c + (-1,0), c + (1,dy), c + (-1,dy)).filter(_.isWalkable)
        case Diagonal(dx,dy) => Seq(c + (0,dy), c + (dx,0), c + (0,2*dy), c + (2*dx,0)).filter(_.isWalkable)
      }
      peripheral ++ c.walkableCone(radius, dir)
    }

    def walkableDisk(radius: Int)(implicit dungeon: Dungeon): Seq[Coord] =
      if(radius == 0) Nil else c.disk(radius, _.isWalkable)

    def visibleDisk(radius: Int)(implicit dungeon: Dungeon): Seq[Coord] = {
      var barrierEncountered = false
      val f = (c: Coord) => (c.isWalkable, barrierEncountered) match {
        case (true,false) => true
        case (false,false) => { barrierEncountered = true; true }
        case _ => { barrierEncountered = false; false }
      }
      c.disk(radius,f)
    }

    @tailrec
    private def findPathIterative(dest: Coord, openDoors: Boolean, pathQueue: Vector[List[Coord]], visited: Set[Coord])(implicit dungeon: Dungeon): List[Coord] = pathQueue match {
      case (path@(c :: _)) +: _ if c == dest => path.reverse.tail

      case (c :: cs) +: paths =>
        def walkable(c: Coord): Boolean = c.isWalkable || (openDoors && dungeon.isDoor(c))
        val moves = c.adjacent.filter(it => walkable(it) && !visited.contains(it))
        val newPaths = moves.sortBy(_.manhattanDistance(c)).map(_ :: c :: cs) // sortBy prioritizes cardinal moves over diagonal moves.
        findPathIterative(dest, openDoors, paths ++ newPaths, visited ++ moves)

      case _ => List()
    }

    def findPath(dest: Coord, openDoors: Boolean = false)(implicit dungeon: Dungeon): List[Coord] =
      findPathIterative(dest, openDoors, Vector(List(c)), Set(c))

    def nextOnPathTo(dest: Coord, openDoor: Boolean = false)(implicit dungeon: Dungeon): Option[Coord] =
      findPath(dest,openDoor).headOption

    def inBounds : Boolean =
      c.x >= 0 && c.x < width && c.y >= 0 && c.y < height
  }
}
