package knave.dungeon

private class InnerFloor(var color : String)

private class InnerWall(var color : String)

private class InnerDoor(var color : String, var open : Boolean)

case class Floor(color : String)

case class Wall(color : String)

case class Door(color : String, open : Boolean)