package knave.world

import knave.game.Action
import knave.world.dungeon.{Coord, Dungeon, Room}
import knave.world.enemy.Enemy
import knave.world.item.Item
import knave.world.player.Player

import scala.util.Random

abstract class World(d : Dungeon) {

  final val dungeon = d

  final val rng = d.rng

  val player : Player

  private val enemies = collection.mutable.Map[Int,Enemy]()

  private var id = 0

  protected def nextId = id

  final def enemy(id : Int) : Option[Enemy] =
    enemies.get(id)

  final def addEnemy(enemy : Enemy) : Unit = {
    enemies. += ((id, enemy))
    id += 1
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

  final def checkCollision(c : Coord, openDoor : Boolean = false) : Collision =
    if(c == player.pos) PlayerCollision
    else {
      val enemyPair = enemies.find(_._2.pos == c)
      if(enemyPair.isDefined) EnemyCollision(enemyPair.get._1)
      else if(openDoor && dungeon.isDoor(c)) NoCollision
      else if(dungeon.isWalkable(c)) NoCollision
      else OutOfBounds
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

  def openWorld(d : Dungeon) : World = new EmptyWorld(d)

  def standardWorld(d : Dungeon) : World = new StandardWorld(d)
}

sealed trait Collision
case object PlayerCollision extends Collision
case class EnemyCollision(id : Int) extends Collision
case object NoCollision extends Collision
case object OutOfBounds extends Collision