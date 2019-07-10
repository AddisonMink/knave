package knave.world

import knave.world.dungeon.{BSPDungeon, Coord, Dungeon, Room}
import knave.world.dungeon.Dungeon._
import knave.world.enemy.{Enemy, LostAcolyte}
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.{Knife, Staff, Weapon}

import scala.util.Random

private object WorldGenHelpers {

  def copyPlayerToDungeon(d: Dungeon, room: Room, player: Player): Player = {
    implicit val (dungeon, rng) = (d, d.rng)
    player.pos = room.randomCoord
    player.fieldOfVision = player.pos.visibleDisk(player.vision).toSet
    player.visitedTiles = player.fieldOfVision
    player.ascended = false
    player
  }

  case class DungeonPartition(startRoom: Room, treasureRoom: Room, combatRooms: Seq[Room], stairsRoom: Room)

  def partitionDungeon(dungeon: Dungeon): DungeonPartition = {
    dungeon.rooms.sortBy(_.area) match {
      case r1 :: Nil => DungeonPartition(r1,r1,Nil,r1)
      case r1 :: r2 :: Nil => DungeonPartition(r1,r2,Nil,r2)
      case r1 :: r2 :: rs => DungeonPartition(r1,r2,rs, dungeon.rng.shuffle(rs).head)
    }
  }

  def placeItems(room: Room, weapons: Seq[Weapon])(implicit rng: Random): Seq[(Coord,WeaponItem)] = {
    rng.shuffle(room.contents).zip(weapons).map(it => (it._1, WeaponItem(it._2, it._1))).toSeq
  }
}

protected object Level_1 {

  def apply(seed: Int): World = {
    import WorldGenHelpers._

    // Create dungeon and rng.
    implicit val dungeon = BSPDungeon(seed)
    implicit val rng = dungeon.rng

    // Partition rooms.
    val partition = partitionDungeon(dungeon)

    // Create stairs.
    dungeon.createStairs(partition.stairsRoom.randomCoord)

    // Create player.
    val player = new Player(partition.startRoom.randomCoord)
    player.refreshFieldOfVision

    // Place a knife in the treasure room.
    val items = placeItems(partition.treasureRoom, Seq(new Knife)).toMap

    // In each combat room, place 1 cursed wretch for every 50 tiles, minimum 1.
    val enemies = {
      var id = 0
      partition.combatRooms.map(r => {
        id += 1; (id, new LostAcolyte(id,r.randomCoord,r))
      }).toMap
    }

    new InnerWorld(1,dungeon,player,enemies,items)
  }
}