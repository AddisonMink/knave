package knave.world.player.weapon

import knave.display.Palette.{lightGray}

class Knife extends Weapon {
  override val name: String = "knife"
  override val color: String = lightGray
  override val maxDurability: Int = 10
  override var durability: Int = maxDurability
  override val attackDamage: Int = 20
  override val attackCost: Int = 1

  override val special: SpecialAttack = Ray(5, attackDamage, attackCost * 2)
}
