package knave.world.dungeon

import scala.collection.mutable.ListBuffer
import knave.world.dungeon.Size.{height, width}

import scala.collection.mutable

private class DefaultDungeon extends Dungeon {

  private val color = "#D3D3D3"
  private val darkColor = "#A9A9A9"

  private val doorColor = "orange"
  private val doorDarkColor = "#FF8C00"

  private val xMiddle = width / 2
  private val yMiddle = height / 2

  private val room1 = Rectangle(1, 1, width / 2 - 2, height - 2)
  private val room2 = Rectangle(width / 2, 1, width / 2 - 1, height - 2)

  val floors = (room1.fill ++ room2.fill).map((_,new InnerFloor(color, darkColor))).toMap

  val (doorCoord, door) = (Coord(width / 2 - 1, height / 2), new InnerDoor(doorColor, doorDarkColor, false))

  val walls = {
    val ws = new ListBuffer[(Coord,InnerWall)]
    for(x <- 0 until width)
      for(y <- 0 until height) {
        val c = Coord(x,y)
        if (!floors.contains(c) && doorCoord != c) ws += ((c, new InnerWall(color,darkColor)))
      }
    ws.toMap
  }

  override def isFloor(c: Coord): Boolean = floors.contains(c)

  override def floorAt(c: Coord): Option[Floor] = floors.get(c).map(f => Floor(f.color, f.darkColor))

  override def isWall(c: Coord): Boolean = walls.contains(c)

  override def wallAt(c: Coord): Option[Wall] = walls.get(c).map(w => Wall(w.color, w.darkColor))

  override def isDoor(c: Coord): Boolean = c == doorCoord

  override def doorAt(c: Coord): Option[Door] = if (c == doorCoord) Some(Door(door.color, door.darkColor, door.open)) else None

  override def openDoor(c: Coord): Unit = if (c == doorCoord) door.open = true

  override val rooms = List(Room.createShapeRoom(List(room1)), Room.createShapeRoom(List(room2)))
}