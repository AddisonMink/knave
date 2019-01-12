package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.Player

private class DefaultWorld extends World {

  val dungeon = Dungeon.createDefaultDungeon

  val player = new Player(Coord(5,5))
}
