package knave.world.player.weapon

import knave.game.AttackOnEnemy

trait Weapon {

  val name : String

  val color : String

  val maxDurability : Int

  var durability : Int

  val attackDamage : Int

  val attackCost : Int

  final def attack(id : Int) : AttackOnEnemy =
    AttackOnEnemy(id, attackDamage)
}
