package knave.world.dungeon

import Size._
import knave.display.Palette._

private class OpenDungeon extends Dungeon {

  private val rect = Rectangle(1, 1, width - 2, height - 2)

  for(x <- 0 until width)
    for(y <- 0 until height)
      tileArray(x)(y) = new InnerWall(lightGray, darkGray)

  for(c <- rect.fill)
    tileArray(c.x)(c.y) = new PlainFloor(lightGray,darkGray)

  override def rooms: List[Room] = List(ShapeRoom(List(rect)))
}
