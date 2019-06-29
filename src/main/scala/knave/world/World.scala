package knave.world

import knave.display.Palette
import knave.game.{Action, Fast, Slow, SpotPlayer}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import knave.world.enemy.Enemy
import knave.world.item.Item
import knave.world.player.Player

import scala.annotation.tailrec

sealed trait World {

  val depth: Int

  implicit val dungeon: Dungeon

  val player: Player

  implicit val rng = dungeon.rng

  def enemy(id : Int) : Option[Enemy]

  def addEnemy(enemy : Enemy) : Unit

  def getEnemies : Seq[Enemy]

  def destroyEnemy(id : Int) : Unit

  def addItem(i : Item) : Unit

  def itemAt(c : Coord) : Option[Item]

  def removeItemAt(c : Coord) : Unit

  def getItems : Iterable[Item]

  var logs: Seq[Log] = Seq()

  var round = 0

  final def speedRound: Boolean = round % 3 == 0

  final def checkCollision(c : Coord) : Collision =
    if(c == player.pos) PlayerCollision
    else {
      val maybeEnemy = getEnemies.find(_.pos == c)
      if(maybeEnemy.isDefined) EnemyCollision(maybeEnemy.get.id)
      else if(c.isWalkable || dungeon.doorAt(c).exists(_.open)) NoCollision
      else BarrierCollision
    }

  @tailrec
  protected final def applyActions(actions: Seq[Action]): Unit = actions match {
    case a +: as => applyActions(a.updateWorld(this) ++ as)
    case Seq() =>
  }

  final def run(playerActions: Seq[Action]): Unit = {
    if(speedRound) {
      val (slowEnemies, nonSlowEnemies) = getEnemies.partition(_.speed == Slow)
      val fastEnemies = nonSlowEnemies.filter(_.speed == Fast)

      applyActions(playerActions ++ nonSlowEnemies.flatMap(_.act(this)))
      applyActions(fastEnemies.flatMap(_.act(this)))
      applyActions(slowEnemies.map(e => SpotPlayer(e.id)))
    } else {
      applyActions(playerActions ++ getEnemies.flatMap(_.act(this)))
    }
    round += 1
  }
}

protected sealed class InnerWorld(val depth: Int, implicit val dungeon : Dungeon, val player: Player, es: Map[Int,Enemy], is: Map[Coord,Item]) extends World {

  private var enemies = es

  private var items = is

  override def enemy(id: Int): Option[Enemy] =
    enemies.get(id)

  override def addEnemy(enemy: Enemy): Unit =
    enemies = enemies + ((enemy.id, enemy))

  override def destroyEnemy(id: Int): Unit =
    enemies = enemies - id

  override def getEnemies: Seq[Enemy] =
    enemies.values.toSeq

  override def addItem(i: Item): Unit =
    items = items + ((i.pos, i))

  override def itemAt(c: Coord): Option[Item] =
    items.get(c)

  override def getItems: Iterable[Item] =
    items.values

  override def removeItemAt(c: Coord): Unit =
    items = items - c
}

object World {
  def apply(seed: Int) = Level_1(seed)
}

sealed trait Collision
case object PlayerCollision extends Collision
case class EnemyCollision(id : Int) extends Collision
case object NoCollision extends Collision
case object BarrierCollision extends Collision

sealed trait Log
case class PlainLog(msg: String, color: String = Palette.white) extends Log
case class AttackOnPlayerLog(name: String, damage: Int) extends Log
case class AttackOnEnemyLog(name: String, damage: Int, percentHealth: Double) extends Log