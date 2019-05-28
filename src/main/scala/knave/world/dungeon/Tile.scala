package knave.world.dungeon

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

private class PlainFloor(color : String, darkColor : String) extends InnerFloor(color ,darkColor) {
  val symbol = "."
}

private class Corpse(color : String, darkColor : String) extends InnerFloor(color, darkColor) {
  override val symbol = "%"
}

private class Stairs(color : String, darkColor : String) extends InnerFloor(color, darkColor) {
  override val symbol: String = "<"
}

private class InnerWall(var color : String, var darkColor : String) extends InnerTile {
  val symbol = "#"
  def tile = Wall(color,darkColor,symbol)
}

private class InnerDoor(var color : String, var darkColor : String, var open : Boolean) extends InnerTile {
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