package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.Player

import scala.util.Random

private class RandomRoomsWorld(seed : Int) extends World {

  private val rng = new Random(seed)

  val dungeon = Dungeon.createRandomRoomsDungeon(seed)

  private val playerPos = dungeon.rooms.head.randomCoord(rng)
  val player = new Player(playerPos)
}
