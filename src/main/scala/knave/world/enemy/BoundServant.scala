package knave.world.enemy
import knave.game._
import knave.world.{NoCollision, World}
import knave.world.dungeon.{Coord, Room}
import knave.display.Palette.white

import scala.util.Random

class BoundServant(i : Int, c : Coord, rng : Random, room : Room) extends Enemy {

  private var dest = c

  override val id: Int = i

  override var pos: Coord = c

  override val maxHp: Int = 20

  override var hp: Int = maxHp

  override val symbol: Char = 's'

  override val color: String = white

  override val name: String = "bound servant"

  private val attackDamage = 10

  override val blood: Int = 1

  override val vision: Int = 3

  override var speed: Speed = Slow

  override protected val canOpenDoors: Boolean = false

  override def flavorText: String =
    """ A human who was slain while scaling the tower.
      | Its soul is now bound to the tower, guarding it until the end of time.
    """.stripMargin

  override def onAlert: Unit = {
    speed = Fast
  }

  override def onHidden: Unit = {
    speed = Slow
  }

  private def move(c : Coord) : Vector[Action] = {
    val m = EnemyMove(id,c,false)
    Vector(m)
  }

  private def attack(c : Coord) : Vector[Action] = {
    val a = AttackOnPlayer(name, attackDamage)
    Vector(a)
  }

  override def act(w: World): Vector[Action] =
    if(w.player.hidden) normalBehavior(w)
    else alertedBehavior(w)

  private def normalBehavior(w : World): Vector[Action] =
    if(pos == dest) {
      dest = room.randomCoord(rng)
      Vector()
    }
    else
      w.dungeon.findPath(pos,dest).headOption.filter(w.checkCollision(_) == NoCollision) match {
        case Some(c) => Vector(EnemyMove(id,c,false))
        case None => {
          dest = room.randomCoord(rng)
          Vector()
        }
      }

  private def alertedBehavior(w : World) : Vector[Action] = {
    if(w.dungeon.castRay(pos, w.player.pos, vision*2)) {
      val c = pos.nextCoord(w.player.pos).get
      if(pos.distance(w.player.pos) == 1) attack(c)
      else move(c)
    }
    else normalBehavior(w)
  }
}
