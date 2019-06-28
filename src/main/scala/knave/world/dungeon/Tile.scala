package knave.world.dungeon

import knave.display.Palette._

protected sealed trait InnerTile {
  def symbol : String
  var color : String
  var darkColor : String
  def tile: Tile
}

abstract private class InnerFloor(var color : String, var darkColor : String) extends InnerTile {
  val symbol : String
  def tile = Floor(color,darkColor,symbol)
}

private class PlainFloor(color : String = lightGray, darkColor : String = darkGray) extends InnerFloor(color ,darkColor) {
  val symbol = "."
}

private class Corpse(color : String = bloodColor, darkColor : String = darkBloodColor) extends InnerFloor(color, darkColor) {
  override val symbol = "%"
}

private class Stairs(color : String = lightGray, darkColor : String = darkGray) extends InnerFloor(color, darkColor) {
  override val symbol: String = "<"
}

private class InnerWall(var color : String = lightGray, var darkColor : String = darkGray) extends InnerTile {
  val symbol = "#"
  def tile = Wall(color,darkColor,symbol)
}

private class InnerDoor(var color : String = orange, var darkColor : String = darkOrange, var open : Boolean = false) extends InnerTile {
  def symbol: String = if(open) "/" else "+"
  def tile = Door(color,darkColor,open,symbol)
}

sealed trait Tile {
  val color: String
  val darkColor: String
  val symbol: String
}
case class Floor(color : String, darkColor : String, symbol : String) extends Tile
case class Wall(color : String, darkColor : String, symbol: String) extends Tile
case class Door(color : String, darkColor : String, open : Boolean, symbol: String) extends Tile