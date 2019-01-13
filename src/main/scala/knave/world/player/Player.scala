package knave.world.player

import knave.world.dungeon.Coord
import knave.world.player.weapon.{Fist, Knife, Weapon}

class Player(c : Coord) {

  val vision = 8

  var pos = c

  val maxHp = 100

  var hp = maxHp

  private var equippedWeapon : Option[Weapon] = Some(new Knife)

  def weapon : Weapon =
    equippedWeapon.getOrElse(Fist)

  def destroyWeapon : Unit =
    equippedWeapon = None
}
