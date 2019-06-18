package knave.world.dungeon
import scala.util.Random

class Room(i: Int, coords: Iterable[Coord]) {

  val id = i

  val contents = coords.toSet

  val area = coords.size

  def randomCoord(implicit rng: Random): Coord = {
    val n = rng.nextInt(contents.size)
    contents.slice(n,n+1).last
  }
}

object Room {
  type RoomId = Int

  type RoomGraph = Map[RoomId,Seq[RoomId]]
}