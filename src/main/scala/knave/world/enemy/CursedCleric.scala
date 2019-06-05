package knave.world.enemy
import knave.game._
import knave.game.Action.ActionAlternative
import knave.world.World
import knave.world.dungeon.{Coord, Room}
import knave.world.dungeon.Dungeon._
import knave.display.Palette.white
import knave.world.player.weapon.{Staff, Weapon}

import scala.util.Random

class CursedCleric(i : Int, c : Coord, rand : Random, r : Room) extends WanderingEnemy with PursuingEnemy {
  override protected var dest: Coord = c
  override val id: Int = i
  override var pos: Coord = c
  override val maxHp: Int = 40
  override val fortifiedHp: Int = maxHp + 20
  override var hp: Int = maxHp
  override val symbol: Char = 'c'
  override val color: String = white
  override val name: String = "cursed cleric"
  override val blood: Int = 2
  override val vision: Int = 5
  override var speed: Speed = Normal
  override val drop: Weapon = new Staff
  override val dropRate: Double = 1.0
  override protected val canOpenDoors: Boolean = false
  override protected val room: Room = r
  override protected val rng: Random = rand
  override protected val attackDamage: Int = 20
  override protected val attackRange: Int = 2

  override def flavorText: String =
    """ A warrior cleric who helped lead the final, ill-fated assault on the tower.
      | Although his soul has been enslaved, he still tends to his followers.
      | He heals nearby enemies and fights with the long iron staff of his order.
    """.stripMargin

  private val healAmount = 10

  private def healWretches(w : World) : Vector[Action] = {
    import w.dungeon
    val wretches = w.getEnemies.filter(e => e.isInstanceOf[CursedWretch] && pos.hasWalkableLineTo(e.pos,vision))
    wretches.map(e => HealEnemy(e.id,healAmount)).toVector
  }

  override protected def normalBehavior(w: World): Vector[Action] =
    healWretches(w) ++ wander(w)

  override protected def alertedBehavior(w: World): Vector[Action] =
    healWretches(w) ++ (pursue(w) tryOrElse wander(w))
}
