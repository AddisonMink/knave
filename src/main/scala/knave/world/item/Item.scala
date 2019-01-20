package knave.world.item

import knave.world.dungeon.Coord
import knave.world.player.weapon.Weapon

sealed trait Item {
  val pos : Coord

  def name : String

  val description : String
}

case class WeaponItem(weapon : Weapon, pos : Coord) extends Item {

  val symbol = "&#8224;"

  val name = weapon.name

  private val flavorText: String =
    """ One of the worn daggers carried by the lesser servants of the tower.
      | It's brittle, but sharp and balanced for throwing.
    """.stripMargin

  val description = s"${name}\n${flavorText}"
}
