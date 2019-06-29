package knave.world.enemy

import knave.game.{Action, EnemyMove, Speed}
import knave.world.{NoCollision, World}
import knave.world.dungeon.{Coord, Direction, Room}
import knave.world.dungeon.Dungeon._
import knave.display.Palette._
import knave.world.player.weapon.Weapon

import scala.util.Random

abstract class Enemy(val id: Int, c: Coord, protected val room: Room) {

  // Immutable Values
  val maxHp : Int

  val fortifiedHp : Int

  val symbol : Char

  val color : String

  val name : String

  val flavorText : String

  val blood : Int

  def vision : Int

  protected val canOpenDoors : Boolean

  val maybeDrop: Option[(Double,Weapon)] = None

  // Variables
  var pos: Coord = c

  var facing : Direction = Direction(0,0)

  var hp : Int = maxHp

  var fieldOfVision : Set[Coord] = Set()

  def speed : Speed

  var awareness: Awareness = Unaware

  var lastKnownPlayerPos: Option[Coord] = None

  // Methods
  def act(w: World): Seq[Action]

  // Final methods.
  final def spotPlayer(w: World): Seq[Action] = {
    if(fieldOfVision.contains(w.player.pos)) {
      println("ALERT!")
      awareness = Alerted
      lastKnownPlayerPos = Some(w.player.pos)
      Seq()
    }
    else Seq()
  }

  protected final def goToDestination(w: World, dest: Coord): Seq[Action] = {
    import w.dungeon
    if(pos == dest) Seq()
    else pos.nextOnPathTo(dest).filter(w.checkCollision(_) == NoCollision) match {
      case Some(c) => Vector(EnemyMove(id,c,canOpenDoors))
      case None => Seq()
    }
  }

  protected final def investigate(w: World): Seq[Action] = lastKnownPlayerPos match {
    case None => Seq()
    case Some(dest) => goToDestination(w,dest) tryOrElse { awareness = Unaware; Seq() }
  }

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

  final val fullDescription : String =
    s"${description}" +
      s"\n${flavorText}"
}

sealed trait Awareness
case object Unaware extends Awareness
case object Cautious extends Awareness
case object Alerted extends Awareness
