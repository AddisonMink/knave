package knave.world.dungeon

import scala.collection.mutable.ListBuffer
import Size._
import knave.display.Palette._

import scala.collection.mutable
import scala.util.Random

private class HubDungeon(seed : Int) extends Dungeon(seed) {

  private val minHubWidth = 3
  private val maxHubWidth = 5
  private val minHubHeight = 2
  private val maxHubHeight = 3
  private val minHubDistance = 5

  private val minSideWidth = 2
  private val maxSideWidth = 4
  private val minSideHeight = 2
  private val maxSideHeight = 3

  // Make every tile a wall.
  for(x <- 0 until width)
    for(y <- 0 until height)
      tileArray(x)(y) = new InnerWall(lightGray,darkGray)

  // TODO After other performance improvments, go back to drop random selection and see if it hurts performacne to badly.
  /*
    Find as many points as possible that are far enough away from each other so that a maximum hub width rectangle can be placed around each point
    and every rectangle will be at least minHubDistance away from each other.
   */
  private val hubPoints = {
    val points = new ListBuffer[Coord]

    val validPoints = new mutable.HashSet[Coord]
    for(x <- maxHubWidth until (width - maxHubWidth))
      for(y <- maxHubHeight until (height - maxHubHeight))
        validPoints += Coord(x,y)
    Random.shuffle(validPoints)

    while(validPoints.nonEmpty) {
      val point = validPoints.head
      validPoints -= point
      points += point

      for(x <- (point.x - (maxHubWidth - 1) - minHubDistance) to (point.x + maxHubWidth + minHubDistance))
        for(y <- (point.y - (maxHubHeight - 1) - minHubDistance) to (point.y + maxHubHeight + minHubDistance))
          validPoints -= Coord(x,y)
    }

    points.sortBy(_.x).toList
  }

  // Build the hub rectangles around each hub points. Keep both a list of hub rectangles and a map of hub points to hub rectangles.
  private val (hubMap, hubRects) : (Map[Coord,Rectangle], List[Rectangle]) = {
    val m = new mutable.HashMap[Coord,Rectangle]()
    val rs = new ListBuffer[Rectangle]
    for(c <- hubPoints) {
      val w = rng.nextInt(maxHubWidth - minHubWidth) + minHubWidth
      val h = rng.nextInt(maxHubHeight - minHubHeight) + minHubHeight
      val x = c.x - (w - 1)
      val y = c.y - (h - 1)
      val rect = Rectangle(x,y,w*2,h*2)
      m += ((c,rect))
      rs += rect
    }
    (m.toMap, rs.toList)
  }

  // Map the hub rectangles onto the dungeon.
  for(r <- hubRects)
    for(x <- r.x until r.x + r.width)
      for(y <- r.y until r.y + r.height)
        tileArray(x)(y) = new PlainFloor(lightGray,darkGray)

  // Compute up to 200 random rectangles that don't intersect with each other or the hub rectangles.
  private val sideRects = {
    val rects = new ListBuffer[Rectangle]
    var tries = 200
    while(tries > 0) {
      val w = rng.nextInt(maxSideWidth - minSideWidth) + minSideWidth
      val h = rng.nextInt(maxSideHeight - minSideHeight) + minSideHeight
      val x = rng.nextInt(width - 2 - w*2) + 1
      val y = rng.nextInt(height - 2 - h*2) + 1
      val rect = Rectangle(x, y, w*2, h*2)

      if(!(rects.exists(r => Shape.intersects(r,rect) || Shape.diagonalAdjacent(r,rect)) || hubRects.exists(r => Shape.intersects(r,rect) || Shape.diagonalAdjacent(r,rect))))
        rects += rect
      tries -= 1
    }
    rects.toList
  }

  // Create a triangulation graph of the hub points.
  private var hubEdges : Set[(Coord,Coord)] = {
    val edges = new mutable.HashSet[(Coord,Coord)]()
    var p1 = hubPoints.head
    var p2 = hubPoints.tail.head
    edges += ((p1,p2))
    for(c <- hubPoints.drop(2)) {
      edges += ((p1,c), (p2,c))
      p1 = p2
      p2 = c
    }
    edges.toSet
  }

  private val corridors : List[List[Coord]] = {
    val corrs = new ListBuffer[List[Coord]]
    val corr = new ListBuffer[Coord]

    def placeCorridors(edges : Iterable[(Coord,Coord)]) : Unit = {
      def handleIntersection(c1 : Coord, hubPoint : Coord, c2 : Coord, es : List[(Coord,Coord)]) : Unit = {
        lazy val edge1Exists = hubEdges.contains((c1,hubPoint)) || hubEdges.contains((hubPoint,c1))
        lazy val edge2Exists = hubEdges.contains((hubPoint,c2)) || hubEdges.contains((hubPoint,c2))
        if(edge1Exists && edge2Exists)
          placeCorridors(es)
        else if(edge1Exists){
          hubEdges = hubEdges + ((hubPoint,c2))
          placeCorridors(((hubPoint,c2)) :: es)
        }
        else if(edge2Exists) {
          hubEdges = hubEdges + ((hubPoint,c2))
          placeCorridors((c1,hubPoint) :: es)
        }
        else {
          hubEdges = hubEdges + ((hubPoint,c2))
          hubEdges = hubEdges + ((hubPoint,c2))
          placeCorridors((c1,hubPoint) :: ((hubPoint,c2)) :: es)
        }
      }

      edges match {
        case Nil => ()
        case (c1,c2) :: es => {
          val rect1 = hubMap.get(c1).get
          val rect2 = hubMap.get(c2).get

          lazy val horizontalOverlap = rect1.x >= rect2.x && rect1.x < rect2.x + rect2.width || rect2.x >= rect1.x && rect2.x < rect1.x + rect1.width
          lazy val verticalOverlap = rect1.y >= rect2.y && rect1.y < rect2.y + rect2.height || rect2.y >= rect1.y && rect2.y < rect1.y + rect1.height

          if(horizontalOverlap) {
            val x = {
              val intersection = (rect1.x until rect1.x + rect1.width).intersect(rect2.x until rect2.x + rect2.width)
              val len = intersection.length
              if(len == 1) intersection.head else intersection(len / 2)
            }

            val corridor = Coord(x,c1.y).lineTo(Coord(x,c2.y)).dropWhile(rect1.contains(_)).takeWhile(!rect2.contains(_))
            val intersectingHubPoint = corridor.map(c => hubPoints.find(hubMap.get(_).get.contains(c))).filter(_.nonEmpty).map(_.get).headOption
            if(intersectingHubPoint.nonEmpty)
              handleIntersection(c1,intersectingHubPoint.get,c2,es)
            else {
              for(c <- corridor)
                corr += c

              corrs += corr.toList
              corr.clear
              placeCorridors(es)
            }
          }

          else if(verticalOverlap) {
            val y = {
              val intersection = (rect1.y until rect1.y + rect1.height).intersect(rect2.y until rect2.y + rect2.height)
              val len = intersection.length
              if(len == 1) intersection.head else intersection(len / 2)
            }

            val corridor = Coord(c1.x,y).lineTo(Coord(c2.x,y)).dropWhile(rect1.contains(_)).takeWhile(!rect2.contains(_))

            val intersectingHubPoint = corridor.map(c => hubPoints.find(hubMap.get(_).get.contains(c))).filter(_.nonEmpty).map(_.get).headOption
            if(intersectingHubPoint.nonEmpty)
              handleIntersection(c1,intersectingHubPoint.get,c2,es)
            else {
              for(c <- corridor)
                corr += c

              corrs += corr.toList
              corr.clear
              placeCorridors(es)
            }
          }

          else {
            val joint = Coord(c2.x,c1.y)
            val horizontal = c1.lineTo(joint).dropWhile(rect1.contains(_))
            val vertical = joint.lineTo(c2).takeWhile(!rect2.contains(_))

            lazy val horizontalIntersectingHubPoint = horizontal.map(c => hubPoints.find(hubMap.get(_).get.contains(c))).filter(_.nonEmpty).map(_.get).headOption
            lazy val verticalIntersectingHubPoint = vertical.map(c => hubPoints.find(hubMap.get(_).get.contains(c))).filter(_.nonEmpty).map(_.get).headOption

            if(horizontalIntersectingHubPoint.nonEmpty)
              handleIntersection(c1,horizontalIntersectingHubPoint.get,c2,es)
            else if(verticalIntersectingHubPoint.nonEmpty)
              handleIntersection(c1,verticalIntersectingHubPoint.get,c2,es)
            else {
              for(c <- horizontal)
                corr += c
              for(c <- vertical)
                corr += c
              corrs += corr.toList
              corr.clear
              placeCorridors(es)
            }
          }
        }
      }
    }

    placeCorridors(hubEdges.toList)
    corrs.toList
  }

  val sideRectsToInclude = {
    val (rectsAdjacentToHubs, otherRects) = sideRects.partition(r => hubRects.exists(Shape.adjacent(_,r)))
    val rectsIntersectedByCorridors = corridors.flatMap(corr => otherRects.filter(rect => corr.exists(rect.contains(_))))

    val startingRects = rectsAdjacentToHubs ++ rectsIntersectedByCorridors

    def addAdjacentRects(complex : List[Rectangle], rects : List[Rectangle]) : List[Rectangle] = {
      if(rects.nonEmpty) {
        val (adjacent, nonAdjacent) = rects.partition(r => complex.exists(Shape.adjacent(_, r)))
        if(adjacent.nonEmpty)
          addAdjacentRects(adjacent ++ complex, nonAdjacent)
        else complex
      }
      else complex
    }
    addAdjacentRects(startingRects, otherRects).distinct
  }

  for(r <- sideRectsToInclude)
    for(x <- r.x until r.x + r.width)
      for(y <- r.y until r.y + r.height)
        tileArray(x)(y) = new PlainFloor(lightGray,darkGray)

  val trimmedCorridors = corridors.flatMap(corr => {
    val corrs = new ListBuffer[List[Coord]]
    val cs = new ListBuffer[Coord]
    for(c <- corr)
      if(isFloor(c)) {
        corrs += cs.toList
        cs.clear
      }
      else
        cs += c

    corrs += cs.toList
    corrs.toList
  })

  for(corr <- trimmedCorridors)
    for(c <- corr)
      tileArray(c.x)(c.y) = new PlainFloor(lightGray,darkGray)

  val doors = trimmedCorridors.flatMap(corr => {
    def isValidDoor(c : Coord) : Boolean = {
      val vertical = Seq(c.copy(y = c.y - 1), c.copy(y = c.y + 1))
      val horizontal = Seq(c.copy(x = c.x - 1), c.copy(x = c.x + 1))
      vertical.forall(isFloor) && horizontal.forall(!isFloor(_)) || vertical.forall(!isFloor(_)) && horizontal.forall(isFloor)
    }

    corr match {
      case Nil => List()
      case c :: Nil => if (isValidDoor(c)) List(c) else List()
      case c :: _ :: Nil => if(isValidDoor(c)) List(c) else List()
      case cs => {
        val door1 = cs.find(isValidDoor)
        val door2 = cs.reverse.find(isValidDoor)
        (door1, door2) match {
          case (Some(d1), Some(d2)) => if (d1.distance(d2) <= 1) List(d1) else List(d1, d2)
          case _ => List()
        }
      }
    }
  })
  for(c <- doors) tileArray(c.x)(c.y) = new InnerDoor(orange,darkOrange,false)

  override def rooms: List[Room] = {
    def fillSpace(start : Coord) : Set[Coord] = {
      val space = mutable.Set[Coord](start)
      var frontier = Stream(start)
      do {
        frontier = frontier.flatMap(_.cardinalAdjacent).filter(isFloor(_)).filter(!space.contains(_))
        space ++= frontier
      } while(frontier.nonEmpty)
      space.toSet
    }

    hubRects.foldLeft(List[Set[Coord]]())((rooms, hub) => {
      val origin = Coord(hub.x,hub.y)
      if(rooms.exists(_.contains(origin)))
        rooms
      else
        fillSpace(origin) :: rooms
    }).map(SetRoom(_))
  }
}































