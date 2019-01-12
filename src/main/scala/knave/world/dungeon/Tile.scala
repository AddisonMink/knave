package knave.world.dungeon

private sealed trait Tile

private class InnerFloor(var color : String, var darkColor : String) extends Tile

private class InnerWall(var color : String, var darkColor : String) extends Tile

private class InnerDoor(var color : String, var darkColor : String, var open : Boolean) extends Tile

case class Floor(color : String, darkColor : String)

case class Wall(color : String, darkColor : String)

case class Door(color : String, darkColor : String, open : Boolean)