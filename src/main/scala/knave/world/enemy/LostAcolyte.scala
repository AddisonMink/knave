package knave.world.enemy

import knave.display.Palette
import knave.game._
import knave.world.World
import knave.world.dungeon.{Coord, Room}
import knave.world.player.weapon.Weapon

class LostAcolyte(override val id: Int, c: Coord, override val room: Room)
  extends Enemy(id,c,room) with AttackingEnemy {

  override val maxHp: Int = 20
  override var hp: Int = maxHp
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
  override val canOpenDoors: Boolean = false

  override val flavorText: String = "[Flavor text goes here.]"

  override val attackDamage: Int = 10

  override def act(w: World): Action = awareness match {
    case Unaware => patrol >> spot
    case Cautious => (investigate | patrol) >> spot
    case Alerted => (attack(w) | chase) >> spot
  }
}
