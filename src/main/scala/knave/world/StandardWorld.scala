package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.enemy.CursedWretch
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.Knife

import scala.collection.mutable.ListBuffer

private class StandardWorld(d : Dungeon) extends World(d) {

  private val (startRoom, treasureRoom, combatRooms) = dungeon.rooms.sortBy(_.area) match {
    case r1 :: Nil => (r1,r1,Nil)
    case r1 :: r2 :: Nil => (r1,r2,Nil)
    case r1 :: r2 :: rs => (r1,r2,rs)
  }

  // Place player in the smallest room.
  override val player: Player = {
    val c = startRoom.randomCoord(rng)
    new Player(c,d)
  }

  // Place a knife next to the player.
  items += {
    val c = startRoom.randomCoordExcept(Seq(player.pos),rng).getOrElse(player.pos)
    (c, WeaponItem(new Knife, c))
  }

  // Place a knife in the treasure room.
  items += {
    val c = treasureRoom.randomCoord(rng)
    (c, WeaponItem(new Knife, c))
  }

  // Place 1 bound servant in each room for each 35 tiles in that room, minimum 1.
  for(r <- combatRooms) {
    val cs = new ListBuffer[Coord]
    val numMonsters = 1 + (r.area - 50) / 50
    for(_ <- 0 until numMonsters) {
      val id = nextId
      for( c <- r.randomCoordExcept(cs,rng))
        addEnemy(new CursedWretch(id,c,rng,r))
    }
  }

  // Place stairs in the lsat combat room.
  val r = combatRooms.last
  dungeon.createStairs(r.randomCoord(rng))
}
