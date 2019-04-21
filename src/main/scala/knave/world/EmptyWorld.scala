package knave.world
import knave.world.dungeon.{Dungeon}
import knave.world.player.Player

import scala.util.Random

private class EmptyWorld(d : Dungeon, p : Option[Player] = None) extends World(d) {

  override val player: Player = {
    val c = d.rooms.head.randomCoord(new Random)
    p match {
      case Some(player) => player.copyToNewDungeon(c,d)
      case None => new Player(c,d)
    }
  }
}
