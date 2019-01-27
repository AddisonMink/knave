package knave.world

import knave.world.dungeon.Dungeon
import knave.world.enemy.BoundServant
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.Knife

private class StandardWorld(d : Dungeon) extends World(d) {

  private val (startRoom, treasureRoom, combatRooms) = dungeon.rooms.sortBy(_.area) match {
    case r1 :: Nil => (r1,r1,Nil)
    case r1 :: r2 :: Nil => (r1,r2,Nil)
    case r1 :: r2 :: rs => (r1,r2,rs)
  }

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

  // Place a knife in the treasure room.
  items += {
    val c = treasureRoom.randomCoord(rng)
    (c, WeaponItem(new Knife, c))
  }

  // Place a Bound Servant in each of the combat rooms.
  for(r <- combatRooms) {
    val id = nextId
    val c = r.randomCoord(rng)
    val enemy = new BoundServant(id,c,rng,r)
    addEnemy(enemy)
  }
}
