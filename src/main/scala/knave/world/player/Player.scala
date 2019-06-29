package knave.world.player

import knave.game.{Normal, Speed}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import knave.world.player.weapon.{Fist, Weapon}

sealed class Player(var pos : Coord) {

  var ascended : Boolean = false

  val vision = 16

  var fieldOfVision : Set[Coord] = Set()

  var visitedTiles = fieldOfVision

  val maxHp = 100

  var hp = maxHp

  var speed : Speed = Normal

  private var equippedWeapon : Option[Weapon] = None

  def weapon : Weapon =
    equippedWeapon.getOrElse(Fist)

  def destroyWeapon : Unit =
    equippedWeapon = None

  def equipWeapon(w : Weapon) : Unit =
    equippedWeapon = Some(w)

  val inventory : Array[Option[Weapon]] = Array(None,None,None)

  def refreshFieldOfVision(implicit dungeon: Dungeon): Unit = {
    fieldOfVision = pos.visibleDisk(vision).toSet
    visitedTiles ++= fieldOfVision
  }
}

