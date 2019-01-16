package knave.world.dungeon

private sealed trait Tile {
  def symbol : String
  var color : String
  var darkColor : String
}

abstract private class InnerFloor(var color : String, var darkColor : String) extends Tile {
  val symbol : String
}

private class PlainFloor(color : String, darkColor : String) extends InnerFloor(color ,darkColor) {
  override val symbol = "."
}

private class Corpse(color : String, darkColor : String) extends InnerFloor(color, darkColor) {
  override val symbol = "%"
}

private class InnerWall(var color : String, var darkColor : String) extends Tile {
  val symbol = "#"
}

private class InnerDoor(var color : String, var darkColor : String, var open : Boolean) extends Tile {
  def symbol = if(open) "/" else "+"
}

case class Floor(color : String, darkColor : String, symbol : String)

case class Wall(color : String, darkColor : String, symbol: String)

case class Door(color : String, darkColor : String, open : Boolean, symbol: String)