package knave.world.player.weapon

class Knife extends Weapon {
  override val name: String = "knife"
  override val maxDurability: Int = 10
  override var durability: Int = maxDurability
  override val attackDamage: Int = 20
  override val attackCost: Int = 1
}
