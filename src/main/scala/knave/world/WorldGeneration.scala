package knave.world

import knave.world.dungeon.{Coord, Dungeon, Room}
import knave.world.dungeon.Dungeon._
import knave.world.enemy.{CursedCleric, CursedWretch, Enemy, LesserAngel}
import knave.world.item.WeaponItem
import knave.world.player.Player
import knave.world.player.weapon.{Knife, Staff, Weapon}

import scala.util.Random

sealed trait EnemyT {
  val pos: Coord
  val room: Room
}
case class CursedWretchT(pos: Coord, room: Room) extends EnemyT
case class CursedClericT(pos: Coord, room: Room) extends EnemyT

private object WorldGenHelpers {

  def makeDungeon(seed: Int): (Dungeon, Random) = {
    val dungeon = Dungeon(seed)
    (dungeon, dungeon.rng)
  }

  def copyPlayerToDungeon(d: Dungeon, room: Room, player: Player): Player = {
    implicit val (dungeon, rng) = (d, d.rng)
    player.pos = room.randomCoord
    player.fieldOfVision = player.pos.visibleDisk(player.vision).toSet
    player.visitedTiles = player.fieldOfVision
    player.ascended = false
    player.hidden = true
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

  def populateCombatRooms(rooms: Seq[Room], populateCombatRoom: Room => Seq[EnemyT])(implicit rng: Random): Seq[(Int,Enemy)] = {
    rooms.flatMap(populateCombatRoom).zipWithIndex.map(_ match {
      case (CursedWretchT(pos,room), id) => (id, new CursedWretch(id,pos,rng,room))
      case (CursedClericT(pos,room), id) => (id, new CursedCleric(id,pos,rng,room))
    })
  }
}

protected object Level_1 {

  def apply(seed: Int): World = {
    import WorldGenHelpers._

    // Create dungeon and rng.
    implicit val (dungeon, rng) = makeDungeon(seed)

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

      def populateCombatRoom(room: Room): Seq[CursedWretchT] = {
        val numEnemies = if(room.area <= 50) 1 else room.area / 50
        val cs = rng.shuffle(room.contents).take(numEnemies).toSeq
        cs.map(CursedWretchT(_,room))
      }

      populateCombatRooms(partition.combatRooms, populateCombatRoom).toMap
    }

    new InnerWorld(1,dungeon,player,enemies,items)
  }
}



protected object Levels2And3 {

  def apply(seed: Int, oldWorld: World): World = {
    import WorldGenHelpers._

    // Create dungeon and rng.
    implicit val (dungeon, rng) = makeDungeon(seed)

    // Partition rooms.
    val partition = partitionDungeon(dungeon)

    // Create stairs.
    dungeon.createStairs(partition.stairsRoom.randomCoord)

    // Create player.
    val player = copyPlayerToDungeon(dungeon,partition.startRoom,oldWorld.player)

    // Place a knife in the treasure room.
    val items = {
      val weapons = if(rng.nextBoolean) Seq(new Knife) else Seq(new Staff)
      placeItems(partition.treasureRoom, weapons).toMap
    }

    // In each combat room, place 1 cursed wretch for every 50 tiles, minimum 1.
    // If there are 4 or more cursed wretches in a single room, replace 1 of them
    // with a cursed cleric.
    val enemies = {

      def populateCombatRoom(room: Room): Seq[EnemyT] = {
        val numEnemies = if(room.area <= 50) 1 else room.area / 50
        val cs = rng.shuffle(room.contents).take(numEnemies).toSeq
        if(numEnemies >= 4)
          CursedClericT(cs.head,room) +: cs.tail.map(CursedWretchT(_,room))
        else
          cs.map(CursedWretchT(_,room))
      }

      populateCombatRooms(partition.combatRooms, populateCombatRoom).toMap
    }

    val depth = oldWorld.depth + 1

    new InnerWorld(depth,dungeon,player,enemies,items)
  }
}

protected object Levels4And5 {

  def apply(seed: Int, oldWorld: World): World = {
    import WorldGenHelpers._

    // Create dungeon and rng.
    implicit val (dungeon, rng) = makeDungeon(seed)

    // Partition rooms.
    val partition = partitionDungeon(dungeon)

    // Create stairs.
    dungeon.createStairs(partition.stairsRoom.randomCoord)

    // Create player.
    val player = copyPlayerToDungeon(dungeon,partition.startRoom,oldWorld.player)

    // Place a knife in the treasure room.
    val items = {
      val weapons = if(rng.nextBoolean) Seq(new Knife) else Seq(new Staff)
      placeItems(partition.treasureRoom, weapons).toMap
    }

    // In each combat room, place 1 cursed wretch for every 50 tiles, minimum 1.
    // If there are 4 or more cursed wretches in a single room, replace 1 of them
    // with a cursed cleric.

    // Place a lesser angel in the treasure room.
    val enemies = {

      def populateCombatRoom(room: Room): Seq[EnemyT] = {
        val numEnemies = if(room.area <= 50) 1 else room.area / 50
        val cs = rng.shuffle(room.contents).take(numEnemies).toSeq
        if(numEnemies >= 4)
          CursedClericT(cs.head,room) +: cs.tail.map(CursedWretchT(_,room))
        else
          cs.map(CursedWretchT(_,room))
      }

      val es = populateCombatRooms(partition.combatRooms, populateCombatRoom).toMap

      val e = {
        val id = es.keys.max + 1
        val pos = partition.treasureRoom.randomCoord
        new LesserAngel(id,pos,rng,partition.treasureRoom)
      }

      es + ((e.id, e))
    }

    val depth = oldWorld.depth + 1

    new InnerWorld(depth,dungeon,player,enemies,items)
  }
}