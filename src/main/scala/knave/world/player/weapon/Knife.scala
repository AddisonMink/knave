package knave.world.player.weapon

import knave.display.Palette.{veryLightGray}

class Knife extends Weapon {
  override val name: String = "knife"
  override val symbol: String = "&#8224;"
  override val color: String = veryLightGray
  override val maxDurability: Int = 6
  override var durability: Int = maxDurability
  override val attackDamage: Int = 20
  override val attackCost: Int = 1

  override val flavorText: String =
    """ One of the worn daggers carried by the lesser servants of the tower.
      | It's brittle, but sharp and balanced for throwing.
    """.stripMargin

  override val special: SpecialAttack = Ray(5, attackDamage, attackCost * 2)
}
