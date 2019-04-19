package knave.world.enemy
import knave.game._
import knave.world.{NoCollision, World}
import knave.world.dungeon.{Coord, Room}
import knave.world.player.weapon.{Axe, Weapon}
import knave.display.Palette._

import scala.util.Random

class LesserAngel(i : Int, c : Coord, rand : Random, r : Room) extends Enemy {
  override val id: Int = i
  override var pos: Coord = c
  override val maxHp: Int = 60
  override val fortifiedHp: Int = maxHp + 20
  override var hp: Int = maxHp
  override val symbol: Char = 'a'
  override val color: String = cyan
  override val name: String = "lesser angel"
  override val blood: Int = 4
  override val vision: Int = 2
  override var speed: Speed = Fast
  override val drop: Weapon = new Axe
  override val dropRate: Double = 30.0
  override protected val canOpenDoors: Boolean = true
  override protected val room: Room = r
  override protected val rng: Random = rand

  override def flavorText: String =
    """ Axe-wielding gargoyle that roosts in the buttresses of the tower.
      | It will pursue you relentlessly as long as any of its comrades can see you.
    """.stripMargin

  private val attackDamage = 30

  private val attack = Vector(AttackOnPlayer(name,attackDamage))

  override def onAlert: Vector[Action] =
    Vector(Log("A chilling scream echoes through the corridors...",yellow))

  override protected def normalBehavior(w: World): Vector[Action] = Vector()

  override protected def alertedBehavior(w: World): Vector[Action] =
    if(pos.distance(w.player.pos) == 1) attack
    else w.dungeon.findPath(pos,w.player.pos,openDoors = true).headOption.filter(c => w.checkCollision(c,true) == NoCollision) match {
      case Some(c) => Vector(EnemyMove(id,c,canOpenDoors))
      case None => normalBehavior(w)
    }
}
