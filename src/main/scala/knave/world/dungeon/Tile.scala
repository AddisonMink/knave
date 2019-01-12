package knave.world.dungeon

private sealed trait Tile

private class InnerFloor(var color : String) extends Tile

private class InnerWall(var color : String) extends Tile

private class InnerDoor(var color : String, var open : Boolean) extends Tile

case class Floor(color : String)

case class Wall(color : String)

case class Door(color : String, open : Boolean)