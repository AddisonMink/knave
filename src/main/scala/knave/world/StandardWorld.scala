package knave.world

import knave.world.dungeon.Dungeon
import knave.world.enemy.BoundServant
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.Knife

import scala.util.Random

private class StandardWorld(d : Dungeon) extends World(d) {

  private val roomsBySize = dungeon.rooms.sortBy(_.area)
  println(dungeon.rooms.length)

  private val (startRoom, otherRooms) = (roomsBySize.head, Random.shuffle(roomsBySize.tail))

  // Place player in the smallest room.
  override val player: Player = {
    val c = startRoom.randomCoord(rng)
    new Player(c)
  }

  // Place a knife next to the player.
  items += {
    val c = startRoom.randomCoordExcept(Seq(player.pos),rng).getOrElse(player.pos)
    (c, WeaponItem(new Knife, c))
  }

  // Place a Bound Servant in each room.
  for(r <- otherRooms) {
    val id = nextId
    val c = r.randomCoord(rng)
    addEnemy(new BoundServant(id,c,rng,r))
  }

  // Place stairs in the last room.
  val lastRoom = if(otherRooms.nonEmpty) otherRooms.head else startRoom
  dungeon.createStairs(lastRoom.randomCoord(rng))
}
