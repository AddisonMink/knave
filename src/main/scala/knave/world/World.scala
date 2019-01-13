package knave.world

import knave.game.Action
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.enemy.Enemy
import knave.world.player.Player

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

  final def checkCollision(c : Coord) : Collision =
    if(c == player.pos) PlayerCollision
    else {
      val enemyPair = enemies.find(_._2.pos == c)
      if(enemyPair.isDefined) EnemyCollision(enemyPair.get._1)
      else if(!dungeon.isWalkable(c)) OutOfBounds
      else NoCollision
    }
}

object World {

  def createDefaultWorld : World = new DefaultWorld

  def createRandomRoomsWorld(seed : Int) : World = new RandomRoomsWorld(seed)
}

sealed trait Collision
case object PlayerCollision extends Collision
case class EnemyCollision(id : Int) extends Collision
case object NoCollision extends Collision
case object OutOfBounds extends Collision