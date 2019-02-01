package knave.world
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.Player

private class EmptyWorld(d : Dungeon) extends World(d) {

  override val player: Player = new Player(d.rooms.head.randomCoord(rng),d)
}
