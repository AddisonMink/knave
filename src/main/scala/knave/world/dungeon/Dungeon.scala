package knave.world.dungeon

import knave.world.dungeon.Room.RoomGraph

import scala.annotation.tailrec
import scala.util.Random

package object Size {
  val height = 20
  val width = 80
}

trait Dungeon {

  val rng: Random

  val graph: RoomGraph

  def rooms : Seq[Room]

  def floorAt(c: Coord): Option[Floor]

  final def isFloor(c: Coord): Boolean = floorAt(c).nonEmpty

  def wallAt(c: Coord): Option[Wall]

  final def isWall(c: Coord): Boolean = wallAt(c).nonEmpty

  def doorAt(c: Coord): Option[Door]

  final def isDoor(c: Coord): Boolean = doorAt(c).nonEmpty

  def openDoor(c: Coord): Unit

  def isStairs(c: Coord): Boolean

  def createStairs(c : Coord) : Unit

  def bloodyTile(c: Coord): Unit

  def createCorpse(c: Coord): Unit
}

object Dungeon {

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
      c.x >= 0 && c.x < Size.width && c.y >= 0 && c.y < Size.height
  }
}
