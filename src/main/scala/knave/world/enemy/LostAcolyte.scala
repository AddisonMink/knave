package knave.world.enemy

import knave.display.Palette
import knave.game._
import knave.world.World
import knave.world.dungeon.{Coord, Room}
import knave.world.player.weapon.{Weapon}

class LostAcolyte(override val id: Int, c: Coord, override protected val room: Room)
  extends Enemy(id,c,room) with WanderingEnemy with AttackingEnemy with PursuingEnemy {

  override val maxHp: Int = 20
  override val fortifiedHp: Int = 30
  override val symbol: Char = 'a'
  override val color: String = Palette.white
  override val name: String = "lost acolyte"
  override val blood: Int = 1
  override def vision: Int = awareness match {
    case Unaware => 5
    case Cautious => 5
    case Alerted => 10
  }
  override def speed: Speed = awareness match {
    case Unaware => Slow
    case Cautious => Normal
    case Alerted => Fast
  }
  override val maybeDrop: Option[(Double, Weapon)] = None
  override protected val canOpenDoors: Boolean = false

  override val flavorText: String = "[Flavor text goes here.]"

  override protected val attackRange: Int = 1
  override protected val attackDamage: Int = 10

  def act(w: World): Seq[Action] = awareness match {
    case Unaware => wander(w)
    case Cautious => investigate(w)

    // TODO Broadcast to enemies in the same room.
    case Alerted => pursue(w)
  }
}
