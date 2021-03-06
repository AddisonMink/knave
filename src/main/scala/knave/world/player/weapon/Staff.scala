package knave.world.player.weapon

import knave.display.Palette.veryLightGray
import knave.game.{Action, HealPlayer}
import knave.world.World

class Staff extends Weapon {
  override val name: String = "staff"
  override val symbol: String = "/"
  override val color: String = veryLightGray
  override val maxDurability: Int = 6
  override var durability: Int = maxDurability
  override val attackDamage: Int = 30
  override val attackCost: Int = 1
  override val special: SpecialAttack = Use(effect,durability,description)

  override val flavorText: String =
    """
      | Long iron staff used by the warrior clerics that once tried to storm the tower.
      | Many years of use as a divine conduit have infused it with healing magic.
    """.stripMargin

  private def effect(w : World) : Vector[Action] = Vector(HealPlayer(durability*10))

  private def description : String = s"Heal for durability * 10 damage. Destroys ${name}."
}
