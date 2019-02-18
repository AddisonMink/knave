package knave.world.enemy
import knave.game._
import knave.world.World
import knave.world.dungeon.{Coord, Room}
import knave.display.Palette.white

import scala.util.Random

class CursedCleric(i : Int, c : Coord, rand : Random, r : Room) extends WanderingEnemy {
  override protected var dest: Coord = c
  override val id: Int = i
  override var pos: Coord = c
  override val maxHp: Int = 4
  override val fortifiedHp: Int = 6
  override var hp: Int = maxHp
  override val symbol: Char = 'c'
  override val color: String = white
  override val name: String = "cursed cleric"
  override val blood: Int = 2
  override val vision: Int = 5
  override var speed: Speed = Normal
  override protected val canOpenDoors: Boolean = false
  override protected val room: Room = r
  override protected val rng: Random = rand

  private val healAmount = 10

  private val attack = AttackOnPlayer(name,20)

  private def move(c : Coord) = EnemyMove(id,c,canOpenDoors)

  override def flavorText: String =
    """ A warrior cleric who helped lead the final, ill-fated assault on the tower.
      | Although his soul has been enslaved, he still tends to his followers.
      | He heals nearby enemies and fights with the long iron staff of his order.
    """.stripMargin

  override protected def normalBehavior(w: World): Vector[Action] = healWretches(w) ++ wander(w)

  override protected def alertedBehavior(w: World): Vector[Action] = {
    if(w.dungeon.castRay(pos,w.player.pos,vision*2)) {
      if(pos.distance(w.player.pos) <= 2)
        Vector(attack)
      else
        Vector(move(pos.nextCoord(w.player.pos).get))
    }
    else
      normalBehavior(w)
  }

  private def healWretches(w : World) : Vector[Action] = {
    val wretches = w.getEnemies.filter(e => e.isInstanceOf[CursedWretch] && w.dungeon.castRay(pos,e.pos,vision))
    wretches.map(e => HealEnemy(e.id,healAmount)).toVector
  }
}
