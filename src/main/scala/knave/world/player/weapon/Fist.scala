package knave.world.player.weapon

object Fist extends Weapon {

  override val name: String = "fist"

  override val maxDurability: Int = 1

  override var durability: Int = 1

  override val attackDamage: Int = 10

  override val attackCost: Int = 0
}
