package knave.world.dungeon

import scala.collection.mutable.ListBuffer
import scala.util.Random
import Size._

private class HubDungeon(seed : Int) extends Dungeon {

  private val rng = new Random(seed)

  private val minHubWidth = 3
  private val maxHubWidth = 5
  private val minHubHeight = 2
  private val maxHubHeight = 3
  private val minHubDistance = 5

  private val minSideWidth = 2
  private val maxSideWidth = 4
  private val minSideHeight = 2
  private val maxSideHeight = 3

  private val hubPoints = {
    val points = new ListBuffer[Coord]


    def placeHubs(validPoints : Set[Coord]) : Unit = {
      if (validPoints.size == 1) points += validPoints.head
      else if (validPoints.nonEmpty) {

        val point = validPoints.drop(rng.nextInt(validPoints.size)).head
        points += point

        val neighborhood = for {
          x <- (point.x - (maxHubWidth - 1) - minHubDistance) to (point.x + maxHubWidth + minHubDistance)
          y <- (point.y - (maxHubHeight - 1) - minHubDistance) to (point.y + maxHubHeight + minHubDistance)
        } yield Coord(x, y)
        //for(c <- neighborhood.filter(c => c.x >= 0 && c.x < width && c.y >= 0 && c.y < height)) tileArray(c.x)(c.y) = new PlainFloor("blue","white")

        placeHubs(validPoints -- neighborhood)
      }
    }

    val coords = (for {
      x <- maxHubWidth until (width - maxHubWidth)
      y <- maxHubHeight until (height - maxHubHeight)
    } yield Coord(x,y)).toSet

    placeHubs(coords)
    points.toList.sortBy(_.x)
  }
  println("Hub points set.")

  private val hubRects : List[Rectangle] =
    hubPoints.map(c => {
      val w = rng.nextInt(maxHubWidth - minHubWidth) + minHubWidth
      val h = rng.nextInt(maxHubHeight - minHubHeight) + minHubHeight
      val x = c.x - (w - 1)
      val y = c.y - (h - 1)
      Rectangle(x,y,w*2,h*2)
    })
  println("Hub rects set.")

  private val sideRects = {
    def loop(rects : List[Rectangle], tries : Int) : List[Rectangle] =
      if(tries == 0) rects
      else {
        val w = rng.nextInt(maxSideWidth - minSideWidth) + minSideWidth
        val h = rng.nextInt(maxSideHeight - minSideHeight) + minSideHeight
        val x = rng.nextInt(width - 2 - w*2) + 1
        val y = rng.nextInt(height - 2 - h*2) + 1
        val rect = Rectangle(x, y, w*2, h*2)
        if(rects.exists(Shape.intersects(_,rect)) || hubRects.exists(Shape.intersects(_,rect)))
          loop(rects, tries - 1)
        else
          loop(rect :: rects, tries - 1)
      }
    loop(List(), 200)
  }
  println("Side rects set.")

  private val hubEdges = {
    val edges = new ListBuffer[(Coord,Coord)]
    def placeEdges(points : List[Coord], p1 : Coord, p2 : Coord) : Unit =
      points match {
        case p :: ps => {
          edges ++= List((p1,p),(p2,p))
          placeEdges(ps, p2, p)
        }
        case _ => ()
      }

    val (firstTwo, rest) = hubPoints.splitAt(2)
    val p1 = firstTwo.head
    val p2 = firstTwo.last
    edges += ((p1,p2))
    placeEdges(rest, p1, p2)
    edges.toList
  }

  for(c <- hubRects.flatMap(_.fill)) tileArray(c.x)(c.y) = new PlainFloor("blue","white")

  for(c <- hubPoints) tileArray(c.x)(c.y) = new PlainFloor("white","white")

  for(c <- sideRects.flatMap(_.fill)) tileArray(c.x)(c.y) = new PlainFloor("red","white")

  for(c <- hubEdges.flatMap(e => e._1.lineTo(e._2).toList)) tileArray(c.x)(c.y) = new PlainFloor("green","white")

  override def rooms: List[Room] = List()
}
