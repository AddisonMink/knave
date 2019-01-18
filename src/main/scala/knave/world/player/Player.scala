package knave.world.player

import knave.game.{Normal, Speed}
import knave.world.dungeon.Coord
import knave.world.player.weapon.{Fist, Knife, Weapon}

sealed class Player(c : Coord) {

  var ascended : Boolean = false

  val vision = 8

  var pos = c

  val maxHp = 100

  var hp = maxHp

  var hidden = true

  var speed : Speed = Normal

  private var equippedWeapon : Option[Weapon] = None

  def weapon : Weapon =
    equippedWeapon.getOrElse(Fist)

  def destroyWeapon : Unit =
    equippedWeapon = None

  def equipWeapon(w : Weapon) : Unit =
    equippedWeapon = Some(w)
}

