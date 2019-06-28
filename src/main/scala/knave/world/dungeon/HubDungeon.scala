package knave.world.dungeon

import knave.world.dungeon.Room.RoomGraph

import scala.annotation.tailrec
import scala.util.Random

object HubDungeon {
  // TODO Require side rects that are adjacent to another rect have an overlap of at least 2.
  // TODO When including side rects, keep track of which corridors are intersected, then create a new graph and compute new corridors for it.

  import Dungeon.{width,height}

  case class Edge(from: Rectangle, to: Rectangle)
  type Corridor = Seq[Coord]
  case class Region(floors: Set[Coord], doors: Set[Coord])
  case class HubRoom(room: Room, doors: Set[Coord])

  def apply(seed: Int, halfWidth: Int = 2, halfHeight: Int = 1, minDistance: Int = 5): Dungeon = {

    implicit val rng = new Random(seed)

    implicit val tileArray = Array.fill[Array[InnerTile]](width)(Array.fill[InnerTile](height)(new InnerWall))

    val hubPoints = computeHubPoints(halfWidth,halfHeight,minDistance)

    val hubRects = computeHubRects(hubPoints,halfWidth,halfHeight)

    val graph = computeGraph(hubRects)

    val corridors = computeCorridors(hubRects,graph)

    val sideRects = computeValidSideRects(computeSideRects(hubRects,halfWidth,halfHeight), hubRects, corridors)

    hubRects.flatMap(_.fill).foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    sideRects.flatMap(_.fill).foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    val finalCorridors = fragmentCorridors(corridors)

    finalCorridors.flatten.foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    val doors = computeDoors(finalCorridors)

    doors.foreach(c => tileArray(c.x)(c.y) = new InnerDoor)

    val rooms = computeRooms(hubRects ++ sideRects)

    //val roomGraph = computeRoomGraph(doors, rooms)

    new InnerDungeon(tileArray, rooms.map(_.room), Map(), rng)
  }

  private def computeHubPoints(halfWidth: Int, halfHeight: Int, minDistance: Int)(implicit rng: Random): Seq[Coord] = {

    val validPoints = rng.shuffle(for {
      x <- halfWidth + 1 until (width - halfWidth - 1)
      y <- halfHeight + 1 until (height - halfHeight - 1)
    } yield Coord(x, y))

    @tailrec
    def loop(points: Seq[Coord], hubs: Seq[Coord] = Nil): Seq[Coord] = points match {
      case p +: ps =>
        val neighborhood = for {
          x <- (p.x - halfWidth - minDistance)  to (p.x +  + minDistance)
          y <- (p.y - halfHeight - minDistance) to (p.y + halfHeight + minDistance)
        } yield Coord(x,y)
        loop(ps.diff(neighborhood), p +: hubs)

      case _ => hubs
    }
    loop(validPoints)
  }

  private def computeHubRects(hubPoints: Seq[Coord], halfWidth: Int, halfHeight: Int)(implicit rng: Random): Seq[Rectangle] = {
    hubPoints.foldLeft(Seq[Rectangle]())((acc, c) => {
      val halfW = rng.nextInt(halfWidth) + 1
      val halfH = rng.nextInt(halfHeight) + 1
      Rect(c.x - halfW, c.y - halfH, halfW*2+1, halfH*2+1) +: acc
    })
  }

  private def computeGraph(hubRects: Seq[Rectangle]): Seq[Edge] = {
    @tailrec
    def loop(rects: Seq[Rectangle], edges: Seq[Edge]): Seq[Edge] = rects match {
      case r1 +: r2 +: r3 +: rs => loop(r2 +: r3 +: rs, Edge(r1,r3) +: Edge(r2,r3) +: edges)
      case r1 +: r2 +: Seq() => Edge(r1,r2) +: edges
      case _ => Nil
    }
    hubRects.sortBy(_.x) match {
      case rects @ r1 +: r2 +: rs => loop(rects, Edge(r1,r2) +: Seq())
      case _ => throw new Exception("Hub Dungeon Generation Error: A graph can't be constructed from fewer than 2 hubs.")
    }
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

    coords.dropWhile(from.contains).takeWhile(!to.contains(_))
  }

  def computeCorridors(hubRects: Seq[Rectangle], graph: Seq[Edge]): Seq[Corridor] = {
    @tailrec
    def loop(edges: Seq[Edge], processed: Set[Edge] = Set(), corridors: Seq[Corridor] = Nil): Seq[Corridor] = edges match {
      case Edge(from,to) +: es if processed.contains(Edge(from,to)) || processed.contains(Edge(to,from)) => loop(es,processed,corridors)
      case Edge(from,to) +: es =>
        val corridor = computeCorridor(from,to)
        corridor.flatMap(c => hubRects.find(_.contains(c))).headOption match {
          case None => loop(es, processed + Edge(from,to) + Edge(to,from), corridor +: corridors)
          case Some(rect) => loop(Edge(from,rect) +: Edge(rect,to) +: es, processed, corridors)
        }

      case _ => corridors
    }
    loop(graph)
  }

  private def computeSideRects(hubRects: Seq[Rectangle], halfWidth: Int, halfHeight: Int)(implicit rng: Random): Seq[Rectangle] = {
    (0 until 200).foldLeft(Seq[Rectangle]())((rects,_) => {
      val halfW = rng.nextInt(halfWidth) + 1
      val halfH = rng.nextInt(halfHeight) + 1
      val x = rng.nextInt(width - 2 - halfW*2) + 1
      val y = rng.nextInt(height - 2 - halfH*2) + 1
      val rect = Rect(x,y,halfW*2+1,halfH*2+1)
      if(rects.exists(r => r.intersects(rect) || r.diagonalAdjacent(rect)) || hubRects.exists(r => r.intersects(rect) || r.diagonalAdjacent(rect)))
        rects
      else
        rect +: rects
    })
  }

  private def computeValidSideRects(sideRects: Seq[Rectangle], hubRects: Seq[Rectangle], corridors: Seq[Corridor]): Seq[Rectangle] = {
    val corridorCoords = corridors.flatten

    val (validRects, unusedRects) = sideRects.partition(r => hubRects.exists(_.cardinalAdjacent(r)) || corridorCoords.exists(r.contains))

    @tailrec
    def loop(unusedRects: Seq[Rectangle], validRects: Seq[Rectangle]): Seq[Rectangle] = {
      val (adjacent, nonAdjacent) = unusedRects.partition(r => validRects.exists(_.cardinalAdjacent(r)))
      if(adjacent.nonEmpty)
        loop(nonAdjacent, adjacent ++ validRects)
      else
        validRects
    }
    loop(unusedRects,validRects)
  }

  private def isWall(c: Coord)(implicit tileArray: Array[Array[InnerTile]]): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerWall]

  private def isFloor(c: Coord)(implicit tileArray: Array[Array[InnerTile]]): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerFloor]

  private def isDoor(c: Coord)(implicit tileArray: Array[Array[InnerTile]]): Boolean = tileArray(c.x)(c.y).isInstanceOf[InnerDoor]

  // Assumes that hub rects and side rects have been mapped onto the tile array but corridors have not.
  private def fragmentCorridors(corridors: Seq[Corridor])(implicit tileArray: Array[Array[InnerTile]]): Seq[Corridor] = {
    @tailrec
    def fragment(corridor: Corridor, fragments: Seq[Corridor] = Nil): Seq[Corridor] = {
      val (frag,rest) = corridor.dropWhile(isFloor).span(!isFloor(_))
      if(frag.nonEmpty)
        fragment(rest, frag +: fragments)
      else fragments
    }
    corridors.flatMap(fragment(_))
  }

  private def computeDoors(corridors: Seq[Corridor])(implicit tileArray: Array[Array[InnerTile]]): Seq[Coord] = {
    def isValidDoor(c: Coord): Boolean =
      c.cardinalAdjacent.filter(isFloor) match {
        case Seq(c1,c2) if c1.x == c2.x || c1.y == c2.y => true
        case _ => false
      }

    val doors = for {
      cs <- corridors
      d1 <- cs.find(isValidDoor).toSeq
      d2 <- cs.reverse.find(isValidDoor).toSeq
      door <- if (d1.manhattanDistance(d2) > 1) Seq(d1,d2) else Seq(d1)
    } yield door

    // Corridors might overlap so make sure that 2 doors from different corridors aren't next to each other.
    doors.foldLeft(Seq[Coord]())((acc, c) => if(acc.exists(_.manhattanDistance(c) == 1)) acc else c +: acc)
  }

  private def computeRegion(start: Coord)(implicit tileArray: Array[Array[InnerTile]]): Region = {
    @tailrec
    def loop(frontier: Set[Coord], floors: Set[Coord] = Set(), doors: Set[Coord] = Set()): Region = {

      val newFrontier = frontier.flatMap(_.cardinalAdjacent).filter(c => !isWall(c) && !floors.contains(c) && !doors.contains(c))

      val (newDoors, newFloors) = newFrontier.partition(isDoor)

      if(newFloors.nonEmpty)
        loop(newFloors, floors ++ newFloors, newDoors ++ doors)
      else Region(floors, doors ++ newDoors)
    }
    loop(Set(start))
  }

  private def computeRooms(rects: Seq[Rectangle])(implicit tileArray: Array[Array[InnerTile]]): Seq[HubRoom] = {
    @tailrec
    def loop(rects: Seq[Rectangle], id: Int = 0, rooms: Seq[HubRoom] = Nil): Seq[HubRoom] = rects match {
      case r +: rs =>
        val Region(floors, doors) = computeRegion(r.midpoint)
        val unusedRects = rs.filterNot(rect => floors.contains(rect.midpoint))
        loop(unusedRects, id+1, HubRoom(new Room(id,floors), doors) +: rooms)
      case _ => rooms
    }
    loop(rects)
  }

  private def computeRoomGraph(doors: Seq[Coord], hubRooms: Seq[HubRoom])(implicit tileArray: Array[Array[InnerTile]]): RoomGraph = {

    // Create a map of each door to all doors it shares a corridor with.
    val doorsConnectedByCorridors = doors.map(d => {
      val Seq(left, right) = d.cardinalAdjacent.filter(isFloor)
      (hubRooms.exists(_.room.contains(left)), hubRooms.exists(_.room.contains(right))) match {
        case (true, true) => (d, Seq(d))
        case (false, true) => (d, computeRegion(left).doors.toSeq)
        case (true,false) => (d, computeRegion(right).doors.toSeq)
        case (false,false) => throw new Exception("Hub Dungeon Generation Error: Neither side of a door was contained in a room.")
      }
    }).toMap

    // Associate each hub room with all doors connected by a corridor to it's doors.
    val expandedHubRooms = hubRooms.map(r => r.copy(doors = r.doors.flatMap(doorsConnectedByCorridors)))

    // If 2 distinct expanded hub rooms share a door, they are connected by an edge.
    val edges = expandedHubRooms.combinations(2).flatMap(pair => {
      val Seq(h1, h2) = pair
      if(h1.room.id != h2.room.id && h1.doors.intersect(h2.doors).nonEmpty) (h1.room.id,h2.room.id) +: (h2.room.id,h1.room.id) +: Seq()
      else Seq()
    }).toSeq

    // Aggregate the edges into map of each room id to the ids of adjacent rooms.
    edges.groupBy(_._1).mapValues(_.map(_._2))
  }
}
