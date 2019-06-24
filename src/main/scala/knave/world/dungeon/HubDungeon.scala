package knave.world.dungeon

import knave.world.dungeon.Room.RoomGraph

import scala.util.Random
import scala.annotation.tailrec

object HubDungeon {
  import Dungeon.{width,height}

  /**
    * A Hub Dungeon should have the following properties:
    * - Densely connected. There should almost always be at least 2 distinct paths between any 2 rooms.
    * - Variety of room sizes.
    * - Short corridors.
    *
    * 1. Randomly choose coords that are some minimum distance away from each other and the bounds of the dungeon.
    *
    * 2. Construct a rectangle around each of these coords to create the hubs.
    *
    * 3. Construct a graph of the hubs like so:
    *    - Sort hubs by x component.
    *    - For hubs 1 and 2 there is an edge 1 <-> 2
    *    - For hub n there are edges (n-1) <-> n and (n-2) <-> n
    *    This graph has the property that there are at least 2 distinct paths between any 2 hubs.
    *
    * 4. Randomly choose up to 200 smaller rectangles that are not intersecting or diagonally adjacent with each other
    *    or with the hubs.
    *
    * 5. Map the hub rectangles and the side rectangles onto a grid.
    *
    * 6. Use the hub graph as a guide construct corridors connecting the hub rectangles.
    *    - If there is an edge x -> y create a main corridor x -> y
    *    - If a corridor x -> y intersects or is adjacent to a rect z, discard the corridor and replace edge x -> y with x -> z and z -> y. If z is a side rect, it is promoted to a hub.
    *    - If a corridor w -> x intersects or is adjacent to a corridor y -> z, discard the corridor and replace it with corridors w -> (y -> z) and (y -> z) -> x
    *    - There can only be 1 corridor corresponding to an edge x -> y.
    *    The corridors imply a new set of hub points. For each edge x -> y associated with a corridor, x and y are hubs.
    *
    * 7. Map the corridors onto the grid.
    *
    * 10. Compute the coords of the doors. A tile is a valid door if it has exactly 2 floors cardinally adjacent to it.
    *
    * 11. Compute rooms. A rooms is the continuous set of coords centered at a hub point and bounded by walls and doors.
    *
    * 12. Compute a graph of the rooms and the corridiors.
    */
  def apply(seed: Int): Dungeon = {
    implicit val rng: Random = new Random(seed)

    val tileArray = Array.ofDim[InnerTile](width,height)

    for {
      x <- 0 until width
      y <- 0 until height
    } yield tileArray(x)(y) = new InnerWall

    val hubRects = computeHubs.map(_.rect)

    for {
      rect <- hubRects
      c <- rect.fill
    } yield tileArray(c.x)(c.y) = new PlainFloor

    val sideRects = computeSideRects(hubRects)

    for {
      rect <- sideRects
      c <- rect.fill
    } yield tileArray(c.x)(c.y) = new PlainFloor

    val graph = computeHubGraph(hubRects)

    val corridors = computeCorridors(graph, hubRects ++ sideRects)

    for {
      corridor <- corridors
      c <- corridor.coords
    } yield tileArray(c.x)(c.y) = new PlainFloor

    val newHubs = corridors.flatMap(c => Seq(c.edge.from, c.edge.to))

    for{
      r <- newHubs
      c <- r.fill
    } yield tileArray(c.x)(c.y) = new PlainFloor

    val doors = computeDoors(tileArray, corridors)

    for{
      c <- doors
    } yield tileArray(c.x)(c.y) = new InnerDoor

    val newHubPoints = corridors.flatMap(c => Seq(c.edge.to.midpoint, c.edge.from.midpoint)).distinct

    val rooms = computeRooms(tileArray, newHubPoints)

    for {
      r <- rooms
      c <- r.contents
    } tileArray(c.x)(c.y) = new Corpse

    val roomGraph = Map[Int,Seq[Int]]()

    new InnerDungeon(tileArray, rooms, roomGraph, rng)
  }

  private case class Hub(x: Int, y: Int) {
    import Hub._

    lazy val neighborhood = for {
      nx <- (x - halfWidth*2 - minXDistance)  to (x + halfWidth*2 + minXDistance)
      ny <- (y - halfHeight*2 - minYDistance) to (y + halfHeight*2 + minYDistance)
    } yield Coord(nx,ny)

    lazy val rect = Rect(x-halfWidth, y-halfHeight, halfWidth*2+1, halfHeight*2+1)
  }

  private object Hub {
    val halfWidth = 2
    val halfHeight = 1
    val minXDistance = 7
    val minYDistance = 5

    def apply(c: Coord): Hub = Hub(c.x,c.y)
  }

  private object SideRect{
    def apply(rng: Random): Rectangle = {
      val x = rng.nextInt(width - 5) + 1
      val y = rng.nextInt(height - 4) + 1
      val w = rng.nextInt(3) + 2
      val h = rng.nextInt(2) + 2
      Rect(x,y,w,h)
    }
  }

  private case class Edge(from: Rectangle, to: Rectangle) {
    lazy val reverse = Edge(to,from)
  }

  private type Graph = Seq[Edge]

  private case class Corridor(edge: Edge, coords: Seq[Coord])

  private class HubRoom(val id: Int, val contents: Set[Coord], val doors: Seq[Coord]) extends Room

  private def computeHubs(implicit rng: Random): Seq[Hub] = {
    import Hub._
    val points = rng.shuffle(for {
      x <- (halfWidth + 1) until (width - halfWidth - 1)
      y <- (halfHeight + 1) until (height - halfHeight - 1)
    } yield Coord(x,y))

    @tailrec
    def loop(points: Seq[Coord], hubs: Seq[Hub] = Nil): Seq[Hub] = points match {
      case p +: ps =>
        val hub = Hub(p)
        loop(ps.diff(hub.neighborhood), hub +: hubs)

      case _ => hubs
    }
    loop(points)
  }

  private def computeHubGraph(hubs: Seq[Rectangle]): Graph = {

    @tailrec
    def loop(rects: Seq[Rectangle], graph: Graph = Nil): Graph = rects match {
      case r1 +: r2 +: r3 +: rest =>
        val newEdges = Edge(r1,r3) +: Edge(r2,r3) +: Nil
        loop(r2 +: r3 +: rest, newEdges ++ graph)

      case r1 +: r2 +: Nil => Edge(r1,r2) +: graph
    }
    loop(hubs.sortBy(_.x))
  }

  private def computeSideRects(hubRects: Seq[Rectangle])(implicit rng: Random): Seq[Rectangle] = {
    (0 until 200).foldLeft(Seq[Rectangle]())((rects, _) => {
      val rect = SideRect(rng)

      val invalid = (hubRects ++ rects).exists(r => {
        val adjacentBy1 = r.cardinalAdjacent(rect) && (r.xOverlap(rect) == 1 || r.yOverlap(rect) == 1)
        r.intersects(rect) || r.diagonalAdjacent(rect) || adjacentBy1
      })

      if(invalid) rects
      else rect +: rects
    })
  }

  private def computeCorridor(from: Rectangle, to: Rectangle): Corridor = {
    val xIntersection = (from.x until from.x + from.width).intersect(to.x until to.x + to.width)
    val yIntersection = (from.y until from.y + from.height).intersect(to.y until to.y + to.height)

    val coords =
      if(xIntersection.nonEmpty) {
        val x = xIntersection(xIntersection.length/2)
        Coord(x,from.midpoint.y).lineTo(Coord(x,to.midpoint.y))
      }
      else if(yIntersection.nonEmpty) {
        val y = yIntersection(yIntersection.length/2)
        Coord(from.midpoint.x,y).lineTo(Coord(to.midpoint.x,y))
      }
      else from.midpoint.ellTo(to.midpoint)

    Corridor(Edge(from,to), coords.dropWhile(from.contains).takeWhile(!to.contains(_)))
  }

  private def findRectIntersection(coords: Seq[Coord], rects: Seq[Rectangle]): Option[Rectangle] = {
    def intersectsCorridor(rect: Rectangle, c: Coord): Boolean = {
      rect.contains(c) || rect.cardinalAdjacent(Rect(c.x,c.y,1,1))
    }

    @tailrec
    def loop(coords: Seq[Coord]): Option[Rectangle] = coords match {
      case c +: cs => rects.find(intersectsCorridor(_,c)) match {
        case None => loop(cs)
        case someRect => someRect
      }
      case _ => None
    }

    if(coords.length >= 2) {
      val middle = coords.drop(1).dropRight(1)
      loop(middle)
    }
    else None
  }

  private def computeCorridors(graph: Graph, rects: Seq[Rectangle]): Seq[Corridor] = {
    @tailrec
    def loop(edges: Seq[Edge], processed: Set[Edge] = Set(), corridors: Seq[Corridor] = Nil): Seq[Corridor] = edges match {
      case e +: es if e.to == e.from || processed.contains(e) || processed.contains(e.reverse) =>
        loop(es, processed, corridors)

      case e +: es =>
        val corridor = computeCorridor(e.from, e.to)

        findRectIntersection(corridor.coords, rects) match {
          case None => loop(es, processed + e, corridor +: corridors)
          case Some(rect) => loop(Edge(e.from, rect) +: Edge(rect, e.to) +: es, processed + e, corridors)
        }

      case _ => corridors
    }
    loop(graph)
  }

  private def computeDoors(tileArray: Array[Array[InnerTile]], corridors: Seq[Corridor]): Seq[Coord] = {
    def isFloor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

    def isValidDoor(c: Coord): Boolean = c.cardinalAdjacent.filter(isFloor) match {
      case Seq(left,right) if left.x == right.x || left.y == right.y => true
      case _ => false
    }

    corridors.foldLeft(Seq[Coord]())((doors, corridor) => {
      (for {
        c1 <- corridor.coords.find(isValidDoor).toSeq
        c2 <- corridor.coords.reverse.find(isValidDoor).toSeq
        door <- if (c1 == c2 || c1.manhattanDistance(c2) == 1) Seq(c1) else Seq(c1, c2)
      } yield door) ++ doors
    })
  }

  private def fill(tileArray: Array[Array[InnerTile]], start: Coord): (Set[Coord], Seq[Coord]) = {
    def isDoor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerDoor]
    def isFloor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

    @tailrec
    def loop(contents: Set[Coord], frontier: Seq[Coord], doors: Seq[Coord] = Nil): (Set[Coord], Seq[Coord]) = {
      val newFrontier = frontier.flatMap(_.cardinalAdjacent).filter(c => !contents.contains(c) && isFloor(c))
      val (newDoors, newFloors) = newFrontier.partition(isDoor)
      if(newFloors.nonEmpty)
        loop(contents ++ newFloors, newFloors, newDoors ++ doors)
      else (contents, newDoors ++ doors)
    }
    loop(Set(start), Seq(start))
  }

  private def computeRooms(tileArray: Array[Array[InnerTile]], hubPoints: Seq[Coord]): Seq[HubRoom] = {
    @tailrec
    def loop(hubPoints: Seq[Coord], id: Int = 0, rooms: Seq[HubRoom] = Nil): Seq[HubRoom] = hubPoints match {
      case h +: hs =>
        val (contents, doors) = fill(tileArray,h)
        val room = new HubRoom(id,contents,doors)
        loop(hs.filterNot(room.contains), id+1, room +: rooms)
      case _ => rooms
    }
    loop(hubPoints)
  }

  private def computeRoomGraph(tileArray: Array[Array[InnerTile]], rooms: Seq[HubRoom], doors: Seq[Coord]): RoomGraph = {
    def isFloor(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

    val connectedRooms = doors.map(c => {
      val Seq(left,right) = c.cardinalAdjacent.filter(isFloor)
      println(left,c,right)
      (rooms.find(_.contains(left)), rooms.find(_.contains(right))) match {
        case (Some(_), None) =>
          val ds = fill(tileArray, right)._2
          ds.flatMap(d => rooms.find(_.doors.contains(d)).map(_.id))

        case (None, Some(_)) =>
          val ds = fill(tileArray, left)._2
          ds.flatMap(d => rooms.find(_.doors.contains(d)).map(_.id))

        case (Some(room1), Some(room2)) => Seq(room1.id,room2.id)

        case (None,None) => throw new Exception("Hub Dungeon Generation Error: Neither side of a door was contained in a room.")
      }
    })

    connectedRooms.groupBy(_.head).mapValues(_.tail).mapValues(_.flatten)
  }
}
