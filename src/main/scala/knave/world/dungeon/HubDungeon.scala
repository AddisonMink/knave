package knave.world.dungeon

import scala.collection.mutable.ListBuffer
import scala.util.Random
import Size._
import knave.display.Palette._

import scala.collection.mutable

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

  for {
    x <- 0 until width
    y <- 0 until height
  } yield tileArray(x)(y) = new InnerWall(lightGray,darkGray)

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

  private val hubMap : Map[Coord,Rectangle] =
    hubPoints.map(c => {
      val w = rng.nextInt(maxHubWidth - minHubWidth) + minHubWidth
      val h = rng.nextInt(maxHubHeight - minHubHeight) + minHubHeight
      val x = c.x - (w - 1)
      val y = c.y - (h - 1)
      (c,Rectangle(x,y,w*2,h*2))
    }).toMap

  private val hubRects = hubMap.values.toList

  for(c <- hubRects.flatMap(_.fill)) tileArray(c.x)(c.y) = new PlainFloor(lightGray,darkGray)

  // TODO DISALLOW HORIZONTAL ADJACENCY
  private val sideRects = {
    def loop(rects : List[Rectangle], tries : Int) : List[Rectangle] =
      if(tries == 0) rects
      else {
        val w = rng.nextInt(maxSideWidth - minSideWidth) + minSideWidth
        val h = rng.nextInt(maxSideHeight - minSideHeight) + minSideHeight
        val x = rng.nextInt(width - 2 - w*2) + 1
        val y = rng.nextInt(height - 2 - h*2) + 1
        val rect = Rectangle(x, y, w*2, h*2)
        if(rects.exists(r => Shape.intersects(r,rect) || Shape.diagonalAdjacent(r,rect)) || hubRects.exists(r => Shape.intersects(r,rect) || Shape.diagonalAdjacent(r,rect)))
          loop(rects, tries - 1)
        else
          loop(rect :: rects, tries - 1)
      }
    loop(List(), 200)
  }

  private var hubEdges : Set[(Coord,Coord)] = {
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
  for(c <- sideRectsToInclude.flatMap(_.fill)) tileArray(c.x)(c.y) = new PlainFloor(lightGray,darkGray)

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
  for(c <- trimmedCorridors.flatten) tileArray(c.x)(c.y) = new PlainFloor(lightGray,darkGray)

  val doors = trimmedCorridors.flatMap(corr =>
    corr match {
      case Nil => List()
      case c :: Nil => if(c.cardinalAdjacent.count(isFloor(_)) == 2) List(c) else List()
      case c :: _ :: Nil => if(c.cardinalAdjacent.count(isFloor(_)) == 2) List(c) else List()
      case cs => {
        val door1 = cs.find(_.cardinalAdjacent.count(isFloor(_)) == 2)
        val door2 = cs.reverse.find(_.cardinalAdjacent.count(isFloor(_)) == 2)
        (door1, door2) match {
          case (Some(d1),Some(d2)) => if(d1.distance(d2) <= 1) List(d1) else List(d1,d2)
          case _ => List()
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































