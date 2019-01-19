package knave.world
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.Player

private class OpenWorld extends World {
  override val dungeon: Dungeon = Dungeon.createOpenDungeon
  override val player: Player = new Player(Coord(10,10))
}
