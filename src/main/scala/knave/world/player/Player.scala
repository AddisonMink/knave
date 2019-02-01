package knave.world.player

import knave.game.{Normal, Speed}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.weapon.{Fist, Weapon}

import scala.collection.mutable

sealed class Player(c : Coord, d: Dungeon) {

  var ascended : Boolean = false

  var pos = c

  val vision = 8

  var fieldOfVision : Set[Coord] = d.circle(pos,vision)

  var visitedTiles = new mutable.HashSet[Coord]
  visitedTiles ++= fieldOfVision

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

