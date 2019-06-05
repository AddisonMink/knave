package knave.world.enemy

import knave.game.{Action, EnemyMove, Speed}
import knave.world.World
import knave.world.dungeon.{Coord, Direction, Room}
import knave.world.dungeon.Dungeon._
import knave.display.Palette._
import knave.world.player.weapon.Weapon

import scala.util.Random

abstract class Enemy {

  val id : Int

  var pos : Coord

  var facing : Direction = Direction(0,0)

  val maxHp : Int

  val fortifiedHp : Int

  var hp : Int

  val symbol : Char

  val color : String

  val name : String

  val blood : Int

  val vision : Int

  var fieldOfVision : Set[Coord] = Set()

  var speed : Speed

  val drop : Weapon

  val dropRate : Double

  protected val canOpenDoors : Boolean

  protected val room : Room

  protected implicit val rng : Random

  def flavorText : String

  def onAlert : Vector[Action] = Vector()

  def onHidden : Vector[Action] = Vector()

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

  final protected def randomMove(rng : Random): EnemyMove = {
    val i = rng.nextInt(4)
    val c = i match {
      case 0 => pos.copy(y = pos.y - 1)
      case 1 => pos.copy(y = pos.y + 1)
      case 2 => pos.copy(x = pos.x - 1)
      case 3 => pos.copy(x = pos.x + 1)
    }
    EnemyMove(id,c,false)
  }

  final def canSeePlayer(w : World) : Boolean = {
    import w.dungeon
    if(w.player.hidden) fieldOfVision.contains(w.player.pos)
    else pos.hasWalkableLineTo(w.player.pos, vision*2)
  }

  def act(w : World) : Vector[Action] =
    if(w.player.hidden) normalBehavior(w) else alertedBehavior(w)

  protected def normalBehavior(w : World) : Vector[Action]

  protected def alertedBehavior(w : World) : Vector[Action]
}
