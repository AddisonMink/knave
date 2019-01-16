package knave.world.enemy
import knave.game.{Action, AttackOnPlayer, EnemyMove}
import knave.world.World
import knave.world.dungeon.Coord

import scala.util.Random

class BoundServant(id : Int, c : Coord, rng : Random) extends Enemy {

  override var pos: Coord = c

  override val maxHp: Int = 20

  override var hp: Int = maxHp

  override val symbol: Char = 's'

  override val color: String = "white"

  override val name: String = "bound servant"

  private val attackDamage = 10

  override val blood: Int = 1

  override def act(w: World): Vector[Action] =
    if(w.dungeon.castRay(pos, w.player.pos)) {
      val c = pos.nextCoord(w.player.pos).get
      if(pos.distance(w.player.pos) == 1) Vector(AttackOnPlayer(name, attackDamage))
      else Vector(EnemyMove(id,c,false))
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
