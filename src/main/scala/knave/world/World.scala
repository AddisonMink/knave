package knave.world

import knave.world.dungeon.Dungeon
import knave.world.player.Player

trait World {

  val dungeon : Dungeon

  val player : Player
}

object World {

  def createDefaultWorld : World = new DefaultWorld

  def createRandomRoomsWorld(seed : Int) : World = new RandomRoomsWorld(seed)
}