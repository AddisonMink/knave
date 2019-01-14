package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.enemy.BoundServant
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.Knife

import scala.util.Random

private class RandomRoomsWorld(seed : Int) extends World {

  private val rng = new Random(seed)

  val dungeon = Dungeon.createRandomRoomsDungeon(seed)

  private val playerPos = dungeon.rooms.head.randomCoord(rng)
  val player = new Player(playerPos)

  for(room <- dungeon.rooms) {
    var c = room.randomCoord(rng)
    while(checkCollision(c) != NoCollision)
      c = room.randomCoord(rng)
    val servant = new BoundServant(nextId, c, rng)
    addEnemy(servant)
  }

  var c = dungeon.rooms.head.randomCoord(rng)
  while(checkCollision(c) != NoCollision)
    c = dungeon.rooms.head.randomCoord(rng)
  val knife = WeaponItem(new Knife, c)
  addItem(knife)
}
