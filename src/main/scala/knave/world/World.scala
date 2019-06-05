package knave.world

import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import knave.world.enemy.Enemy
import knave.world.item.Item
import knave.world.player.Player

abstract class World(d : Dungeon) {

  final implicit val dungeon = d

  final implicit val rng = d.rng

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

  final def checkCollision(c : Coord) : Collision =
    if(c == player.pos) PlayerCollision
    else {
      val enemyPair = enemies.find(_._2.pos == c)
      if(enemyPair.isDefined) EnemyCollision(enemyPair.get._1)
      else if(c.isWalkable || dungeon.doorAt(c).exists(_.open)) NoCollision
      else OutOfBounds
    }
}

// TODO Deprecate this.
object World {

  def openWorld(d : Dungeon, p : Option[Player] = None) : World = new EmptyWorld(d,p)

  def standardWorld(d : Dungeon, p : Option[Player] = None) : World = new StandardWorld(d,p)
}

sealed trait Collision
case object PlayerCollision extends Collision
case class EnemyCollision(id : Int) extends Collision
case object NoCollision extends Collision
case object OutOfBounds extends Collision