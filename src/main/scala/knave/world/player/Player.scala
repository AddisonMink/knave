package knave.world.player

import knave.world.dungeon.Coord
import knave.world.player.weapon.{Fist, Weapon}

class Player(c : Coord) {

  val vision = 8

  var pos = c

  private var equippedWeapon : Option[Weapon] = None

  def weapon = equippedWeapon.getOrElse(Fist)
}
