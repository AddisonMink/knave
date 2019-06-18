package knave.world.dungeon

import scala.util.Random
import Size._
import knave.display.Palette.{bloodColor, darkBloodColor, darkGray, lightGray}
import knave.world.dungeon.Room.RoomGraph

import scala.annotation.tailrec

protected class HubDungeon(val rng: Random, val rooms: Seq[Room], val graph: RoomGraph, corridors: Seq[Coord], doors: Seq[Coord]) extends Dungeon {

  private val tileArray = Array.ofDim[InnerTile](width,height)

  for(x <- 0 until width)
    for(y <- 0 until height)
      tileArray(x)(y) = new InnerWall

  for(r <- rooms)
    for(c <- r.contents)
      tileArray(c.x)(c.y) = new PlainFloor

  for(c <- corridors)
    tileArray(c.x)(c.y) = new PlainFloor

  for(d <- doors)
    tileArray(d.x)(d.y) = new InnerDoor

  override def floorAt(c: Coord): Option[Floor] = tileArray(c.x)(c.y) match {
    case f : InnerFloor => Some(f.tile)
    case _ => None
  }

  override def wallAt(c: Coord): Option[Wall] = tileArray(c.x)(c.y) match {
    case w : InnerWall => Some(w.tile)
    case _ => None
  }

  override def doorAt(c: Coord): Option[Door] = tileArray(c.x)(c.y) match {
    case d : InnerDoor => Some(d.tile)
    case _ => None
  }

  override def openDoor(c: Coord): Unit = tileArray(c.x)(c.y) match {
    case d : InnerDoor => d.open = true
    case _ => ()
  }

  override def isStairs(c: Coord): Boolean = tileArray(c.x)(c.y).isInstanceOf[Stairs]

  override def createStairs(c : Coord) : Unit = tileArray(c.x)(c.y) = new Stairs(lightGray,darkGray)

  override def bloodyTile(c: Coord): Unit = {
    val tile = tileArray(c.x)(c.y)
    tile.color = bloodColor
    tile.darkColor = darkBloodColor
  }

  override def createCorpse(c: Coord): Unit = {
    if(tileArray(c.x)(c.y).isInstanceOf[InnerFloor] && !isStairs(c))
      tileArray(c.x)(c.y) = new Corpse(bloodColor, darkBloodColor)
  }
}

object HubDungeon {

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
    * 5. Combine the hubs and smaller rectangles and aggregate them into complexes of adjacent rectangles.
    *
    * 6. Map the hub graph onto a grid like so:
    *    - For each edge 1 <-> 2 choose a sequence of coords connecting 1 and 2.
    *    - If the sequence of coords intersects or is adjacent to a complex 'c', replace the edge 1 <-> 2 with edges 1 <-> c and c <-> 2.
    *    The sequences of coords are called corridors. Each corridor is associated with an edge in the new graph.
    *
    * 7. Trim the corridors so that they don't overlap with any complexes.
    *    - For corridor i -> j
    *       - Remove all coords that intersect with i or j.
    *       - Remove all but the last coord that is adjacent to i.
    *       - Remove all but the first coord that is adjacent to j.
    *       - If the trimmed corridor is empty, discard it.
    *
    * 8. Choose doors coords by taking the first and last coord of each trimmed corridor.
    *
    * 9. Create a room from each complex connected by the complex graph.
    *
    * 10. Create a room graph from the complex graph.
    *
    * 11. Instantiate dungeon.
    */

  /**
    * Data Structures
    */
  case class HubRect(x: Int, y: Int, width: Int, height: Int, hub: Coord) extends Rectangle

  object HubRect {
    val (minWidth, maxWidth) = (3, 5)
    val (minHeight, maxHeight) = (2, 3)
    val minDistance = 5

    def random(hub: Coord)(implicit rng: Random): HubRect = {
      val w = rng.nextInt(maxWidth - minWidth) + minWidth
      val h = rng.nextInt(maxHeight - minHeight) + minHeight
      val x = hub.x - (w - 1)
      val y = hub.y - (h - 1)
      HubRect(x,y,w,h,hub)
    }
  }

  object SideRect {
    val (minWidth, maxWidth) = (2, 4)
    val (minHeight, maxHeight) = (2, 3)

    def random(implicit rng: Random): Rect = {
      val w = rng.nextInt(maxWidth - minWidth) + minWidth
      val h = rng.nextInt(maxHeight - minHeight) + minHeight
      val x = rng.nextInt(width - 2 - w*2) + 1
      val y = rng.nextInt(height - 2 - h*2) + 1
      Rect(x, y, w*2, h*2)
    }
  }

  case class HubEdge(from: HubRect, fromComplex: Complex, to: HubRect, toComplex: Complex)

  type HubGraph = Seq[HubEdge]

  case class Complex(id: Int, rects: Seq[Rectangle]) {

    def contains(c: Coord): Boolean =
      rects.exists(_.contains(c))

    def rectContaining(c: Coord): Option[Rectangle] =
      rects.find(_.contains(c))

    def adjacent(c: Coord): Boolean =
      rects.exists(_.adjacent(c))

    def cardinalAdjacent(c: Coord): Boolean =
      rects.exists(_.cardinalAdjacent(c))
  }

  case class ComplexEdge(from: Complex, to: Complex)

  type ComplexGraph = Seq[ComplexEdge]

  case class Corridor(edge: ComplexEdge, coords: Seq[Coord])

  /**
    * Algorithm
    */
  def computeHubs(implicit rng: Random): Seq[Coord] = {
    val points = rng.shuffle(for {
      x <- HubRect.maxWidth until (width - HubRect.maxWidth)
      y <- HubRect.maxHeight until (height - HubRect.maxHeight)
    } yield Coord(x,y))

    @tailrec
    def loop(points: Seq[Coord], hubPoints: Seq[Coord] = Nil): Seq[Coord] = points match {
      case newHubPoint +: ps =>
        val hubPointBounds = for {
          x <- (newHubPoint.x - (HubRect.maxWidth - 1) - HubRect.minDistance) to (newHubPoint.x + HubRect.maxWidth + HubRect.minDistance)
          y <- (newHubPoint.y - (HubRect.maxHeight - 1) - HubRect.minDistance) to (newHubPoint.y + HubRect.maxHeight + HubRect.minDistance)
        } yield Coord(x,y)
        loop(ps.diff(hubPointBounds), newHubPoint +: hubPoints)

      case Seq() => hubPoints
    }
    loop(points)
  }

  def computeSideRects(hubRects: Seq[HubRect])(implicit rng: Random): Seq[Rect] = {
    @tailrec
    def loop(tries: Int, rects: Seq[Rect] = Nil): Seq[Rect] = tries match {
      case 0 => rects
      case _ =>
        val rect = SideRect.random

        val intersectsAnotherSideRect = rects.exists(r => r.intersects(rect) || r.diagonalAdjacent(rect))
        val intersectsHubRect = hubRects.exists(r => r.intersects(rect) || r.diagonalAdjacent(rect))

        if(intersectsAnotherSideRect || intersectsHubRect)
          loop(tries-1, rects)
        else
          loop(tries-1, rect +: rects)
    }
    loop(200)
  }

  def computeComplexes(rects: Seq[Rectangle]): Seq[Complex] = {
    @tailrec
    def loop(unusedRects: Seq[Rectangle], complexes: Seq[Complex] = Nil, currentId: Int = 0): Seq[Complex] = unusedRects match {
      case Seq() => complexes
      case r +: rs =>
        @tailrec
        def aggregateAdjacent(agg: Seq[Rectangle], unusedRects: Seq[Rectangle]): (Seq[Rectangle], Seq[Rectangle]) = {
          val (adjacentRects, nonAdjacentRects) = unusedRects.partition(rect => agg.exists(_.cardinalAdjacent(rect)))
          if(adjacentRects.isEmpty)
            (agg, nonAdjacentRects)
          else
            aggregateAdjacent(adjacentRects ++ agg, nonAdjacentRects)
        }

        val (adjacentRects, otherRects) = aggregateAdjacent(Seq(r),rs)
        loop(otherRects, Complex(currentId, adjacentRects) +: complexes, currentId+1)
    }
    loop(rects)
  }

  def computeHubGraph(complexes: Seq[Complex]): HubGraph = {

    val hubComplexes = complexes.flatMap{complex =>
      val hubRects = complex.rects.collect{case r: HubRect => r}
      if(hubRects.nonEmpty)
        hubRects.map((_,complex))
      else Nil
    }

    @tailrec
    def loop(hubs: Seq[(HubRect,Complex)], graph: HubGraph = Nil): HubGraph = hubs match {
      case (r1, c1) +: (r2, c2) +: hub3 +: rest =>
        val newGraph = HubEdge(r1,c1,r2,c2) +: graph
        loop((r2,c2) +: hub3 +: rest, newGraph)

      case (r1, c1) +: (r2, c2) +: Seq() => HubEdge(r1,c1,r2,c2) +: graph

      case _ => graph
    }
    loop(hubComplexes)
  }

  def computeCorridors(graph: HubGraph, complexes: Seq[Complex]): Seq[Corridor] = {

    def computeCoords(from: HubRect, to: HubRect): Seq[Coord] = {
      val xIntersection = (from.x until from.x + from.width).intersect(to.x until to.x + to.width)
      val yIntersection = (from.y until from.y + from.height).intersect(to.y until to.y + to.height)

      if(xIntersection.nonEmpty) {
        val x = xIntersection(xIntersection.length/2)
        Coord(x,from.hub.y).lineTo(Coord(x,to.hub.y))
      }
      else if(yIntersection.nonEmpty) {
        val y = yIntersection(yIntersection.length/2)
        Coord(from.hub.x,y).lineTo(Coord(to.hub.x,y))
      }
      else from.hub.ellTo(to.hub)
    }

    def trimCoords(coords: Seq[Coord], fromComplex: Complex, toComplex: Complex): Seq[Coord] = {
      if(fromComplex.id == toComplex.id) Nil
      else {
        val nonIntersecting = coords.dropWhile(fromComplex.contains).takeWhile(!toComplex.contains(_))
        val (leftAdjacent, right) = nonIntersecting.span(fromComplex.cardinalAdjacent)
        val (middle, rightAdjacent) = right.span(!toComplex.cardinalAdjacent(_))
        leftAdjacent.lastOption.toSeq ++ middle ++ rightAdjacent.headOption.toSeq
      }
    }

    @tailrec
    def findIntermediateComplex(coords: Seq[Coord]): Option[(Rectangle,Complex)] = coords match {
      case c +: cs => complexes.find(_.contains(c)) match {
        case None => findIntermediateComplex(cs)
        case Some(complex) =>
          val rect = complex.rectContaining(c).get
          Some((rect,complex))
      }
      case _ => None
    }

    def loop(edges: Seq[HubEdge], corridors: Seq[Corridor] = Nil): Seq[Corridor] = edges match {
      case HubEdge(from,fromComplex,to,toComplex) +: rest =>
        val coords = computeCoords(from,to)
        val trimmed = trimCoords(coords,fromComplex,toComplex)

        findIntermediateComplex(trimmed) match {
          case None if trimmed.isEmpty =>
            loop(rest, corridors)

          case None =>
            val corridor = Corridor(ComplexEdge(fromComplex,toComplex),trimmed)
            loop(rest, corridor +: corridors)

          case Some((hubRect: HubRect,intermediateComplex)) =>
            val newEdges = HubEdge(from,fromComplex,hubRect,intermediateComplex) +: HubEdge(hubRect,intermediateComplex,to,toComplex) +: Nil
            loop(newEdges ++ rest, corridors)

          case Some((rect, intermediateComplex)) =>
            val hubRect = HubRect(rect.x, rect.y, rect.width, rect.height, rect.midpoint)
            val newEdges = HubEdge(from,fromComplex,hubRect,intermediateComplex) +: HubEdge(hubRect,intermediateComplex,to,toComplex) +: Nil
            loop(newEdges ++ rest, corridors)
        }

      case _ => corridors
    }
    loop(graph)
  }

  def computeRoomGraph(corridors: Seq[Corridor]): RoomGraph = {
    val edges = corridors.flatMap{e =>
      val (id1, id2) = (e.edge.from.id, e.edge.to.id)
      Seq((id1,id2), (id2,id1))
    }
    edges.groupBy(_._1).mapValues(_.map(_._2))
  }

  def computeRooms(corridors: Seq[Corridor]): Seq[Room] = {
    val complexes = corridors.flatMap(c => Seq(c.edge.from, c.edge.to)).distinct
    complexes.map(c => new Room(c.id, c.rects.flatMap(_.fill)))
  }

  def apply(seed: Int): HubDungeon = {
    implicit val rng: Random = new Random(seed)

    val hubRects = computeHubs.map(HubRect.random)

    val sideRects = computeSideRects(hubRects)

    val complexes = computeComplexes(hubRects ++ sideRects)

    val hubGraph = computeHubGraph(complexes)

    val corridors = computeCorridors(hubGraph,complexes)

    val doors = Nil

    val roomGraph = computeRoomGraph(corridors)

    val rooms = computeRooms(corridors)

    new HubDungeon(rng, rooms, roomGraph, Nil, Nil)
  }
}
