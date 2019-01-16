package knave.world

import knave.game.Action
import knave.world.dungeon.{Coord, Dungeon, Room}
import knave.world.enemy.Enemy
import knave.world.item.Item
import knave.world.player.Player

import scala.util.Random

trait World {

  val dungeon : Dungeon

  val player : Player

  protected val enemies = collection.mutable.Map[Int,Enemy]()

  protected var nextId = 0

  final def enemy(id : Int) : Option[Enemy] =
    enemies.get(id)

  final def addEnemy(enemy : Enemy) : Unit = {
    enemies. += ((nextId, enemy))
    nextId += 1
  }

  final def getEnemies : Iterable[Enemy] =
    enemies.values

  final def destroyEnemy(id : Int) : Unit =
    enemies.remove(id)

  protected val items = collection.mutable.Map[Coord,Item]()

  final def addItem(i : Item) : Unit =
    items += ((i.pos, i))

  final def itemAt(c : Coord) : Option[Item] =
    items.get(c)

  final def removeItemAt(c : Coord) : Unit =
    items.remove(c)

  final def getItems : Iterable[Item] =
    items.values

  final def checkCollision(c : Coord) : Collision =
    if(c == player.pos) PlayerCollision
    else {
      val enemyPair = enemies.find(_._2.pos == c)
      if(enemyPair.isDefined) EnemyCollision(enemyPair.get._1)
      else if(!dungeon.isWalkable(c)) OutOfBounds
      else NoCollision
    }

  protected def randomCoordFromRoom(r : Room, rng : Random, tries : Int = 100) : Coord = {
    var c = r.randomCoord(rng)
    var i = tries
    while(checkCollision(c) != NoCollision && i > 0) {
      c = r.randomCoord(rng)
      i -= 1
    }
    c
  }
}

object World {

  def createRandomRoomsWorld(seed : Int) : World = new RandomRoomsWorld(seed)
}

sealed trait Collision
case object PlayerCollision extends Collision
case class EnemyCollision(id : Int) extends Collision
case object NoCollision extends Collision
case object OutOfBounds extends Collision