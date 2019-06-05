package knave.world.enemy
import knave.game._
import knave.world.{World}
import knave.world.dungeon.{Coord, Room}
import knave.game.Action.ActionAlternative
import knave.display.Palette.white
import knave.world.player.weapon.{Knife, Weapon}
import Vector.empty

import scala.util.Random

class CursedWretch(i : Int, c : Coord, rand : Random, r : Room) extends WanderingEnemy with PursuingEnemy {

  override protected var dest: Coord = c

  override val id: Int = i

  override var pos: Coord = c

  override val maxHp: Int = 20

  override val fortifiedHp: Int = maxHp + 10

  override var hp: Int = maxHp

  override val symbol: Char = 'w'

  override val color: String = white

  override val name: String = "cursed wretch"

  override val blood: Int = 1

  override val vision: Int = 3

  override var speed: Speed = Slow

  override val drop: Weapon = new Knife

  override val dropRate: Double = 0.2

  override protected val canOpenDoors: Boolean = false

  override protected val room: Room = r

  override protected val rng: Random = rand

  override protected val attackDamage = 10

  override protected val attackRange = 1

  override def flavorText: String =
    """ A human who was slain while scaling the tower.
      | Its soul is now bound to the tower, guarding it until the end of time.
      | Normally sluggish, it will go berserk at the sight of an intruder.
    """.stripMargin

  override def onAlert: Vector[Action] = {
    speed = Fast
    empty
  }

  override def onHidden: Vector[Action] = {
    speed = Slow
    empty
  }

  override protected def normalBehavior(w: World): Vector[Action] =
    wander(w)

  override protected def alertedBehavior(w: World): Vector[Action] =
    pursue(w) tryOrElse normalBehavior(w)

}
