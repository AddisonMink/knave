package knave.world.enemy
import knave.game._
import knave.world.{NoCollision, World}
import knave.world.dungeon.{Coord, Room}
import knave.display.Palette.white
import knave.world.item.{Item, WeaponItem}
import knave.world.player.weapon.{Knife, Weapon}

import scala.util.Random

class CursedWretch(i : Int, c : Coord, rand : Random, r : Room) extends WanderingEnemy {

  override protected var dest: Coord = c

  override val id: Int = i

  override var pos: Coord = c

  override val maxHp: Int = 20

  override val fortifiedHp: Int = maxHp + 10

  override var hp: Int = maxHp

  override val symbol: Char = 'w'

  override val color: String = white

  override val name: String = "cursed wretch"

  private val attackDamage = 10

  override val blood: Int = 1

  override val vision: Int = 3

  override var speed: Speed = Slow

  override val drop: Weapon = new Knife

  override val dropRate: Double = 0.2

  override protected val canOpenDoors: Boolean = false

  override protected val room: Room = r

  override protected val rng: Random = rand

  override def flavorText: String =
    """ A human who was slain while scaling the tower.
      | Its soul is now bound to the tower, guarding it until the end of time.
      | Normally sluggish, it will go berserk at the sight of an intruder.
    """.stripMargin

  override def onAlert: Vector[Action] = {
    speed = Fast
    Vector()
  }

  override def onHidden: Vector[Action] = {
    speed = Slow
    Vector()
  }

  private def move(c : Coord) : Vector[Action] = {
    val m = EnemyMove(id,c,false)
    Vector(m)
  }

  private val attack : Vector[Action] = {
    val a = AttackOnPlayer(name, attackDamage)
    Vector(a)
  }

  override protected def normalBehavior(w: World) : Vector[Action] = wander(w)

  override protected def alertedBehavior(w : World) : Vector[Action] = {
    if(w.dungeon.castRay(pos, w.player.pos, vision*2)) {
      val c = pos.nextCoord(w.player.pos).get
      if(pos.distance(w.player.pos) == 1) attack
      else move(c)
    }
    else normalBehavior(w)
  }
}
