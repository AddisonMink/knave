package knave.dungeon

import scala.collection.mutable.ListBuffer
import knave.dungeon.Size.{height, width}

import scala.collection.mutable

private class DefaultDungeon extends Dungeon {

  private val color = "#D3D3D3"

  private val doorColor = "orange"

  private val xMiddle = width / 2
  private val yMiddle = height / 2

  val floors = {
    val room1 = Rectangle(1, 1, width / 2 - 2, height - 2).fill
    val room2 = Rectangle(width / 2, 1, width / 2 - 1, height - 2).fill
    (room1 ++ room2).map((_,new InnerFloor(color))).toMap
  }

  val (doorCoord, door) = (Coord(width / 2 - 1, height / 2), new InnerDoor(doorColor, false))

  val walls = {
    val ws = new ListBuffer[(Coord,InnerWall)]
    for(x <- 0 until width)
      for(y <- 0 until height) {
        val c = Coord(x,y)
        if (!floors.contains(c) && doorCoord != c) ws += ((c, new InnerWall(color)))
      }
    ws.toMap
  }

  override def isFloor(c: Coord): Boolean = floors.contains(c)

  override def floorAt(c: Coord): Option[Floor] = floors.get(c).map(f => Floor(f.color))

  override def isWall(c: Coord): Boolean = walls.contains(c)

  override def wallAt(c: Coord): Option[Wall] = walls.get(c).map(w => Wall(w.color))

  override def isDoor(c: Coord): Boolean = c == doorCoord

  override def doorAt(c: Coord): Option[Door] = if (c == doorCoord) Some(Door(door.color, door.open)) else None
}
