package knave.world.dungeon

import scala.annotation.tailrec
import scala.util.Random

object BSPDungeon extends {
  import Dungeon.{height,width}
  import BSPDungeonGen._

  def apply(seed: Int): Dungeon = {

    implicit val rng = new Random(seed)

    implicit val tileArray = Array.fill[Array[InnerTile]](width)(Array.fill[InnerTile](height)(new InnerWall))

    val partitions = computePartitions



    val floors = partitions.flatMap(_.fill)
    floors.foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    new InnerDungeon(tileArray,Nil,Map(),rng)
  }
}

private object BSPDungeonGen {
  import Dungeon.{width,height}

  val minWidth = 5
  val minHeight = 3

  def computePartitions(implicit rng: Random): Seq[Rectangle] = {

    def partitionHorizontally(rect: Rect): Seq[Rect] = {
      // 20
      // dy = 13
      // left(0,13) right(14,6)

      if(rect.width < 2*minWidth+1) Seq(rect)
      else {
        val dx = rng.nextInt(rect.width - minWidth*2) + minWidth
        val left = rect.copy(width = dx)
        val right = rect.copy(x = rect.x + dx + 1, width = rect.width - dx - 1)
        Seq(left,right)
      }
    }

    def partitionVertically(rect: Rect): Seq[Rect] = {
      if(rect.height < 2*minHeight+1) Seq(rect)
      else {
        val dy = rng.nextInt(rect.height - minHeight*2) + minHeight
        val top = rect.copy(height = dy)
        val bottom = rect.copy(y = rect.y + dy + 1, height = rect.height - dy - 1)
        Seq(top,bottom)
      }
    }

    val start = Rect(1,1,width-2,height-2)

    @tailrec
    def loop(parts: Seq[Rect]): Seq[Rect] = {
      val newParts = parts.flatMap(partitionHorizontally).flatMap(partitionVertically)
      if(newParts.length == parts.length) newParts
      else loop(newParts)
    }
    loop(Seq(start))
  }

  // chapel

  // rotunda

  // archive

  // plain
  def computePlainRoom(partition: Rectangle, id: Int): (Room,Rectangle) = {

    null
  }
}
