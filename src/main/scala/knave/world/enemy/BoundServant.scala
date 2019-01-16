package knave.world.enemy
import knave.game.{Action, AttackOnPlayer, EnemyMove, SpotSplayer}
import knave.world.World
import knave.world.dungeon.Coord

import scala.util.Random

class BoundServant(i : Int, c : Coord, rng : Random) extends Enemy {

  override val id: Int = i

  override var pos: Coord = c

  override val maxHp: Int = 20

  override var hp: Int = maxHp

  override val symbol: Char = 's'

  override val color: String = "white"

  override val name: String = "bound servant"

  private val attackDamage = 10

  override val blood: Int = 1

  override val vision: Int = 3

  /*
  Bound Servants move randomly if the player is hidden.
  If the player has been spotted, they will move toward the player and attack if they have line of sight to the player.
  Otherwise, they will move randomly.
   */
  override def act(w: World): Vector[Action] =
    if(w.player.hidden)
      Vector(randomMove(rng))
    else if(w.dungeon.castRay(pos, w.player.pos, vision*2)) {
      val c = pos.nextCoord(w.player.pos).get
      if(pos.distance(w.player.pos) == 1) Vector(AttackOnPlayer(name, attackDamage))
      else Vector(EnemyMove(id,c,false))
    }
    else Vector(randomMove(rng))
}
