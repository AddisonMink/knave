package knave.world.enemy

import knave.game.Action
import knave.world.World
import knave.world.dungeon.Coord

trait Enemy {

  var pos : Coord

  var hp : Int

  val symbol : Char

  val color : String

  val name : String

  def act(w : World) : Vector[Action]
}
