package knave.dungeon

import scala.collection.mutable.ListBuffer
import knave.dungeon.Size.{height,width}

private class DefaultDungeon extends Dungeon {

  private val color = "#D3D3D3"

  private val doorColor = "orange"

  private val floors : Map[Coord,InnerFloor] = {
    val fs = new ListBuffer[(Coord,InnerFloor)]
    for(y <- 1 until (height - 1)) {
      for(x <- 1 until (width / 2 - 1))
        fs += ((Coord(x,y), new InnerFloor(color)))
      for(x <- (width / 2) until (width - 1))
        fs += ((Coord(x,y), new InnerFloor(color)))
    }
    fs.toMap
  }

  private val walls : Map[Coord, InnerWall] = {
    val ws = new ListBuffer[(Coord,InnerWall)]

    for(y <- 0 until height)
      ws += ((Coord(0,y), new InnerWall(color)), (Coord(width - 1, y), new InnerWall(color)))

    for(x <- 1 until (width - 1))
      ws += ((Coord(x,0), new InnerWall(color)), (Coord(x, height - 1), new InnerWall(color)))

    for(y <- 1 until (height / 2))
      ws += ((Coord(width / 2 - 1, y), new InnerWall(color)))
    for(y <- (height / 2 + 1) until (height - 1))
      ws += ((Coord(width / 2 - 1, y), new InnerWall(color)))

    ws.toMap
  }

  private val door = (Coord(width / 2 - 1, height / 2), new InnerDoor(doorColor, false))

  override def isFloor(c: Coord): Boolean = floors.contains(c)

  override def floorAt(c: Coord): Option[Floor] = floors.get(c).map(f => Floor(f.color))

  override def isWall(c: Coord): Boolean = walls.contains(c)

  override def wallAt(c: Coord): Option[Wall] = walls.get(c).map(w => Wall(w.color))

  override def isDoor(c: Coord): Boolean = c == door._1

  override def doorAt(c: Coord): Option[Door] = if (isDoor(c)) Some(Door(door._2.color, door._2.open)) else None
}
