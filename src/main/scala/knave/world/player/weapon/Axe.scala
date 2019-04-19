package knave.world.player.weapon

import knave.display.Palette.darkOrange

class Axe extends Weapon {
  override val name: String = "axe"
  override val symbol: String = "&#915;"
  override val color: String = darkOrange
  override val maxDurability: Int = 3
  override var durability: Int = maxDurability
  override val attackDamage: Int = 40
  override val attackCost: Int = 1

  override val flavorText: String =
    """ Heavy bronze axe wielded by the lesser angels guard the upper reaches of the tower.
      | Can be swung in a circle, damaging everything around you.
    """.stripMargin

  override val special: SpecialAttack = Circle(attackDamage,attackCost)
}
