package knave.world.enemy

import knave.game.{Action, Speed}
import knave.world.{NoCollision, World}
import knave.world.dungeon.{Coord, Direction, Dungeon, Room}
import knave.world.dungeon.Dungeon._
import knave.display.Palette._
import knave.game.EnemyActions._
import knave.game.SharedActions.Fail
import knave.world.player.weapon.Weapon

abstract class Enemy(val id: Int, c: Coord, val room: Room) {

  /**
    * Immutable Values
    */
  val maxHp : Int
  val symbol : Char
  val color : String
  val name : String
  val flavorText : String
  val blood : Int
  def vision : Int
  val canOpenDoors : Boolean
  val maybeDrop: Option[(Double,Weapon)] = None

  /**
    * Dependent Values
    */
  def speed : Speed

  final def description : String = {
    def color(str : String, color : String) =
      "<span style=\"color : " + color + "\">" + str + "</span>"

    val status = hp.toFloat / maxHp.toFloat match {
      case x if x > 1.0 => color("fortified",cyan)
      case x if x == 1.0 => "unharmed"
      case x if x > 0.74 => color("scratched", lightGray)
      case x if x > 0.25 => color("wounded", yellow)
      case x if x > 0 => color("near death", red)
      case _ => color("dead", red)
    }

    s"${name} (${status})"
  }

  final def fullDescription : String =
    s"${description}" +
      s"\n${flavorText}"

  /**
    * Mutable Variables
    */
  var pos: Coord = c
  var facing : Direction = Direction(0,0)
  var hp : Int
  var fieldOfVision : Set[Coord] = Set()
  var awareness: Awareness = Unaware
  var lastKnownPlayerPos: Option[Coord] = None
  var patrolDestination: Option[Coord] = None

  /**
    * Methods
    */
  def act(w: World): Action

  final def refreshFieldOfVision(implicit dungeon: Dungeon): Unit = {
    fieldOfVision = awareness match {
      case Alerted => pos.walkableDisk(vision).toSet
      case _ => pos.enemyConeOfVision(vision,facing).toSet
    }
  }

  val spot = SpotPlayer(id)
  val chase = ChasePlayer(id)
  val patrol = Patrol(id)
  val investigate = Investigate(id)
}

protected trait AttackingEnemy {
  self: Enemy =>
  val attackDamage: Int

  def attack(implicit w: World): Action = {
    if(pos.distance(w.player.pos) <= 1) AttackPlayer(name,attackDamage)
    else Fail
  }
}

sealed trait Awareness
case object Unaware extends Awareness
case object Cautious extends Awareness
case object Alerted extends Awareness
