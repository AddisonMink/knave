package knave.world.enemy
import knave.game.{Action, EnemyMove}
import knave.world.World
import knave.world.dungeon.Coord

import scala.util.Random

class BoundServant(id : Int, c : Coord, rng : Random) extends Enemy {

  override var pos: Coord = c

  override var hp: Int = 20

  override val symbol: Char = 's'

  override val color: String = "white"

  override val name: String = "bound servant"

  override def act(w: World): Vector[Action] =
    if(w.dungeon.castRay(pos, w.player.pos)) {
      val c = pos.nextCoord(w.player.pos).get
      Vector(EnemyMove(id,c,false))
    }
    else {
      val i = rng.nextInt(4)
      val c = i match {
        case 0 => pos.copy(y = pos.y - 1)
        case 1 => pos.copy(y = pos.y + 1)
        case 2 => pos.copy(x = pos.x - 1)
        case 3 => pos.copy(x = pos.x + 1)
      }
      Vector(EnemyMove(id,c,false))
    }
}
