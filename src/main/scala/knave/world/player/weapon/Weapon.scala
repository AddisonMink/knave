package knave.world.player.weapon

import knave.game.{Action, AttackOnEnemy}
import knave.world.World
import knave.world.dungeon.Coord

abstract class Weapon {

  val name : String

  val flavorText : String

  val symbol : String

  val color : String

  val maxDurability : Int

  var durability : Int

  val attackDamage : Int

  val attackCost : Int

  final def attack(id : Int) : AttackOnEnemy =
    AttackOnEnemy(id, attackDamage, true)

  val special : SpecialAttack
}

sealed trait SpecialAttack
case class Ray(range : Int, damage : Int, cost : Int) extends SpecialAttack
case class Use(effect : World => Vector[Action], cost : Int, description : String) extends SpecialAttack
case object NoSpecial extends SpecialAttack
