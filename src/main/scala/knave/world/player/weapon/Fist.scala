package knave.world.player.weapon

object Fist extends Weapon {

  override val name: String = "fist"

  override val symbol: String = ""

  override val color: String = "white"

  override val maxDurability: Int = 1

  override var durability: Int = 1

  override val attackDamage: Int = 10

  override val attackCost: Int = 0

  override val special: SpecialAttack = NoSpecial

  override val flavorText: String =
    """
      | Your bare fists. Hardly adequate for serious combat.
    """.stripMargin
}
