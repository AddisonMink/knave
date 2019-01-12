package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.Player

private class RandomRoomsWorld(seed : Int) extends World {

  val dungeon = Dungeon.createRandomRoomsDungeon(seed)

  val player = new Player(Coord(10,10))
}
