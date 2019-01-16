package knave.world.enemy

import knave.game.Action
import knave.world.World
import knave.world.dungeon.Coord

trait Enemy {

  var pos : Coord

  val maxHp : Int

  var hp : Int

  val symbol : Char

  val color : String

  val name : String

  val blood : Int

  val vision : Int

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

  def act(w : World) : Vector[Action]
}
