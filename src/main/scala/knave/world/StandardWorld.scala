package knave.world

import knave.world.dungeon.{Coord, Dungeon, Room}
import knave.world.enemy.{CursedCleric, CursedWretch, LesserAngel}
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.{Knife, Staff}

import scala.collection.mutable.ListBuffer

private class StandardWorld(d : Dungeon) extends World(d) {

  private val (startRoom, treasureRoom, combatRooms) = dungeon.rooms.sortBy(_.area) match {
    case r1 :: Nil => (r1,r1,Nil)
    case r1 :: r2 :: Nil => (r1,r2,Nil)
    case r1 :: r2 :: rs => (r1,r2,rs)
  }

  private val occupiedCoords = new ListBuffer[Coord]

  // Place player in the smallest room.
  override val player: Player = {
    val c = startRoom.randomCoord(rng)
    occupiedCoords += c
    new Player(c,d)
  }

  // Place a knife next to the player.
  items += {
    val c = startRoom.randomCoordExcept(Seq(player.pos),rng).getOrElse(player.pos)
    (c, WeaponItem(new Knife, c))
  }

  items += {
    val c = startRoom.randomCoordExcept(Seq(player.pos),rng).getOrElse(player.pos)
    (c, WeaponItem(new Staff, c))
  }

  // Place a knife in the treasure room.
  items += {
    val c = treasureRoom.randomCoord(rng)
    (c, WeaponItem(new Knife, c))
  }

  // Place 1 cursed wretch in each room for each 35 tiles in that room, minimum 1. If there are 4 or more wretches in 1 room, replace one of them with a cursed cleric.
  private def placeCursedWretch(r : Room) : Unit = {
    val id = nextId
    r.randomCoordExcept(occupiedCoords,rng).foreach(c => {
      occupiedCoords += c
      addEnemy(new CursedWretch(id,c,rng,r))
    })
  }

  private def placeCursedCleric(r : Room) : Unit = {
    val id = nextId
    r.randomCoordExcept(occupiedCoords,rng).foreach(c => {
      occupiedCoords += c
      addEnemy(new CursedCleric(id,c,rng,r))
    })
  }


  for(r <- combatRooms) {
    val numMonsters = 1 + (r.area - 50) / 50

    if(numMonsters >= 4) {
      placeCursedCleric(r)
      (0 until numMonsters - 1).foreach(_ => placeCursedWretch(r))
    }
    else
      (0 until numMonsters).foreach(_ => placeCursedWretch(r))
  }

  // Place a lesser angel in the treasure room.
  val id = nextId
  addEnemy(new LesserAngel(id,treasureRoom.randomCoord(rng),rng,treasureRoom))

  // Place stairs in the last combat room.
  val r = combatRooms.last
  dungeon.createStairs(r.randomCoord(rng))
}
