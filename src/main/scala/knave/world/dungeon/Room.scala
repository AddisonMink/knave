package knave.world.dungeon
import scala.util.Random

trait Room {

  val id: Int

  val contents: Set[Coord]

  final val area = contents.size

  final def randomCoord(implicit rng: Random): Coord = {
    val n = rng.nextInt(contents.size)
    contents.slice(n,n+1).last
  }

  final def contains(c: Coord): Boolean = contents.contains(c)
}

object Room {
  type RoomId = Int

  type RoomGraph = Map[RoomId,Seq[RoomId]]
}