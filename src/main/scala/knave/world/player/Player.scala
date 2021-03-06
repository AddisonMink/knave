package knave.world.player

import knave.game.{Normal, Speed}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.player.weapon.{Fist, Weapon}

import scala.collection.mutable

sealed class Player(c : Coord, d: Dungeon) {

  var ascended : Boolean = false

  var pos = c

  val vision = 16

  var fieldOfVision : Set[Coord] = d.fieldOfVision(pos,vision)

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

  val inventory : Array[Option[Weapon]] = Array(None,None,None)

  var depth = 1

  def copyToNewDungeon(c : Coord, d : Dungeon): Player = {
    pos = c
    fieldOfVision = d.fieldOfVision(pos,vision)
    visitedTiles = new mutable.HashSet[Coord]
    visitedTiles ++= fieldOfVision
    this
  }
}

