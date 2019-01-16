package knave.world.dungeon

import knave.world.dungeon.Size.{height, width}

import scala.collection.immutable.HashSet.HashSet1
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

private class RandomRoomsDungeon(seed : Int) extends Dungeon {

  private val color = "#D3D3D3"
  private val darkColor = "#808080"

  private val doorColor = "orange"
  private val doorDarkColor = "#FF8C00"

  private val tileArray = Array.ofDim[Tile](width,height)
  for(x <- 0 until width)
    for(y <- 0 until height)
      tileArray(x)(y) = new InnerWall(color, darkColor)

  private val rng = new Random(seed)

  private val min = 3
  private val max = 5
  private def randomDim = rng.nextInt(max) + min
  private def randomX(w : Int) = rng.nextInt(width - 2 - w) + 1
  private def randomY(h : Int) = rng.nextInt(height - 2 - h) + 1

  private val rects = {
    def loop(rects : List[Rectangle], tries : Int) : List[Rectangle] =
      if(tries == 0) rects
      else {
        val (w, h) = (randomDim, randomDim)
        val (x, y) = (randomX(w), randomY(h))
        val rect = Rectangle(x, y, w, h)
        if(rects.exists(Shape.intersects(_,rect))) loop(rects, tries-1) else loop (rect :: rects, tries - 1)
      }
    loop(List(), 125)
  }
  for(c <- rects.flatMap(_.fill))
    tileArray(c.x)(c.y) = new PlainFloor(color, darkColor)

  private val aggregates = {
    def loop(rectangles : List[List[Rectangle]], aggregates : List[List[Rectangle]]) : List[List[Rectangle]] = {
      rectangles match {
        case rs :: rest => {
          val (adjs, nonAdjs) = rest.partition(_.exists(r => rs.exists(Shape.adjacent(_,r))))
          if(adjs.isEmpty) loop(rest, rs :: aggregates)
          else {
            val newRs = rs ++ adjs.flatten
            loop(newRs :: nonAdjs, aggregates)
          }
        }
        case _ => aggregates
      }
    }
    loop(rects.map(List(_)), List())
  }

  private val corridors = {
    def loop(rs : List[Rectangle], corrs : List[List[Coord]]) : List[List[Coord]] =
      rs match {
        case r1 :: r2 :: rest => {
          val x1 = rng.nextInt(r1.width) + r1.x
          val y1 = rng.nextInt(r1.height) + r1.y
          val x2 = rng.nextInt(r2.width) + r2.x
          val y2 = rng.nextInt(r2.height) + r2.y
          val horizontalCorr = (x1 to x2).map(Coord(_,y1)).toList
          val verticalCorr =
            if (y1 < y2) (y1 to y2).map(Coord(x2,_))
            else (y2 to y1).map(Coord(x2,_)).reverse
          val corr = horizontalCorr ++ verticalCorr

          loop(r2 :: rest, corr.filter(c => !tileArray(c.x)(c.y).isInstanceOf[InnerFloor]) :: corrs)
        }
        case _ => corrs
      }
    loop(rects.sortBy(_.x), List())
  }
  for(c <- corridors.flatten)
    tileArray(c.x)(c.y) = new PlainFloor(color, darkColor)

  private val endPoints = corridors.flatMap(_ match {
    case Nil => Nil
    case c :: Nil => List(c)
    case c1 :: c2 :: Nil => List(c1)
    case list => List(list.head, list.last)
  })
  private val doors = endPoints.filter(c => {
    val adjs = List(Coord(c.x,c.y+1), Coord(c.x,c.y-1), Coord(c.x-1,c.y), Coord(c.x+1,c.y))
    adjs.filter(c => tileArray(c.x)(c.y).isInstanceOf[InnerWall]).length == 2
  })
  for(c <- doors)
    tileArray(c.x)(c.y) = new InnerDoor(doorColor, doorDarkColor, false)

  override def rooms: List[Room] = aggregates.map(Room.createShapeRoom(_))

  private val stairsCoord = rooms.last.randomCoord(rng)
  tileArray(stairsCoord.x)(stairsCoord.y) = new Stairs(color, darkColor)

  override def isFloor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

  override def floorAt(c: Coord): Option[Floor] = tileArray(c.x)(c.y) match {
    case f : InnerFloor => Some(Floor(f.color, f.darkColor, f.symbol))
    case _ => None
  }

  override def isWall(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerWall]

  override def wallAt(c: Coord): Option[Wall] = tileArray(c.x)(c.y) match {
    case w : InnerWall => Some(Wall(w.color, w.darkColor, w.symbol))
    case _ => None
  }

  override def isDoor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerDoor]

  override def doorAt(c: Coord): Option[Door] = tileArray(c.x)(c.y) match {
    case d : InnerDoor => Some(Door(d.color, d.darkColor, d.open, d.symbol))
    case _ => None
  }

  override def openDoor(c: Coord): Unit = tileArray(c.x)(c.y) match {
    case d : InnerDoor => d.open = true
    case _ => ()
  }

  override def isStairs(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[Stairs]

  override def bloodyTile(c: Coord): Unit = {
    val tile = tileArray(c.x)(c.y)
    tile.color = bloodColor
    tile.darkColor = darkBloodColor
  }

  override def createCorpse(c: Coord): Unit = {
    if(tileArray(c.x)(c.y).isInstanceOf[InnerFloor])
      tileArray(c.x)(c.y) = new Corpse(bloodColor, darkBloodColor)
  }
}
