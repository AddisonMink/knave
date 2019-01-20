package knave.world.enemy

import knave.game.{Action, EnemyMove, Speed}
import knave.world.World
import knave.world.dungeon.Coord

import scala.util.Random

abstract class Enemy {

  val id : Int

  var pos : Coord

  val maxHp : Int

  var hp : Int

  val symbol : Char

  val color : String

  val name : String

  val blood : Int

  val vision : Int

  var speed : Speed

  def flavorText : String

  def onAlert : Unit = ()

  def onHidden : Unit = ()

  final def description : String = {
    def color(str : String, color : String) =
      "<span style=\"color : " + color + "\">" + str + "</span>"

    val status = hp.toFloat / maxHp.toFloat match {
      case x if x == 1.0 => "unharmed"
      case x if x > 0.74 => color("scratched", "light-gray")
      case x if x > 0.25 => color("wounded", "yellow")
      case _ => color("near death", "red")
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

  final def canSeePlayer(w : World) : Boolean =
    if(w.player.hidden)
      w.dungeon.castRay(pos, w.player.pos, vision)
    else
      w.dungeon.castRay(pos, w.player.pos, vision*2)

  def act(w : World) : Vector[Action]
}
