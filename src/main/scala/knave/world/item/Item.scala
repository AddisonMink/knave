package knave.world.item

import knave.world.dungeon.Coord
import knave.world.player.weapon.Weapon

sealed trait Item {
  val pos : Coord

  def name : String
}

case class WeaponItem(weapon : Weapon, pos : Coord) extends Item {

  val symbol = "&#8224;"

  val name = weapon.name
}
