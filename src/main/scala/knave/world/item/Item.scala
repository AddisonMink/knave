package knave.world.item

import knave.world.dungeon.Coord
import knave.world.player.weapon.Weapon

sealed trait Item {
  val pos : Coord

  def name : String

  val description : String
}

case class WeaponItem(weapon : Weapon, pos : Coord) extends Item {

  val symbol = weapon.symbol

  val name = weapon.name

  val description = s"${name}\n${weapon.flavorText}"
}
