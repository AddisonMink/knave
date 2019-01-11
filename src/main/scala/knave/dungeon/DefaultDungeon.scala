package knave.dungeon

import scala.collection.mutable.ListBuffer
import knave.dungeon.Size.{height,width}

private class DefaultDungeon extends Dungeon {

  private val floors : Map[Coord,InnerFloor] = {
    val fs = new ListBuffer[(Coord,InnerFloor)]
    for(y <- 1 until (height - 1)) {
      for(x <- 1 until (width / 2 - 1))
        fs += ((Coord(x,y), new InnerFloor("light-gray")))
      for(x <- (width / 2 + 1) until (width - 1))
        fs += ((Coord(x,y), new InnerFloor("light-gray")))
    }
    fs.toMap
  }

  override def isFloor(c: Coord): Boolean = floors.contains(c)

  override def floorAt(c: Coord): Option[Floor] = floors.get(c).map(f => Floor(f.color))

  override def isWall(c: Coord): Boolean = false

  override def wallAt(c: Coord): Option[Wall] = None
}
