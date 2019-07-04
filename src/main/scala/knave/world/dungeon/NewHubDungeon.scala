package knave.world.dungeon

import scala.annotation.tailrec
import scala.util.Random

object NewHubDungeon {

  import Dungeon.{width,height}

  private case class Edge(from: Rectangle, to: Rectangle)
  private type Corridor = Seq[Coord]

  def apply(seed: Int): Dungeon = {

    // Initialize.
    implicit val rng = new Random(seed)

    implicit val tileArray = Array.fill[Array[InnerTile]](width)(Array.fill[InnerTile](height)(new InnerWall))

    // Choose the hub rectangles.
    val hubs = computeHubs

    // Compute a graph connecting the hub rectangles.
    val graph = computeGraph(hubs)

    // Compute corridors that map the graph onto the tile array.
    val corridors = computeCorridors(hubs,graph)

    val floors = hubs.flatMap(_.fill) ++ corridors.flatten
    floors.foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    new InnerDungeon(tileArray, Nil, Map(), rng)
  }

  /**
    * Randomly choose coordinates that are some minimum distance away from each other and the boundaries of the dungeon.
    * The coordinates must have odd x and y values. Select as many of these points as possible.
    * Construct a 5x5 rectangle around each chosen coordinate.
    */
  private def computeHubs(implicit rng: Random): Seq[Rectangle] = {
    val validPoints = rng.shuffle(for {
      x <- Stream.range(3, width - 3).filter(_ % 2 == 1)
      y <- Stream.range(3, height - 3).filter(_ % 2 == 1)
    } yield Coord(x,y))

    @tailrec
    def loop(points: Seq[Coord], hubs: Seq[Coord] = Nil): Seq[Coord] = points match {
      case p +: ps =>
        val neighborhood = for {
          x <- Stream.range(p.x - 6, p.x + 6).filter(_ % 2 == 1)
          y <- Stream.range(p.y - 6, p.y + 6).filter(_ % 2 == 1)
        } yield Coord(x,y)
        loop(ps.diff(neighborhood), p +: hubs)

      case _ => hubs
    }
    loop(validPoints).map(c => Rect(c.x-2, c.y-2, 5, 5))
  }

  /**
    * Sort the rectangles by their x coordinates and compute a graph to connect them.
    * - There is an edge between the first 2 rectangles.
    * - There is an edge between the last 2 rectangles.
    * - For ordered rectangles x, y, and z there are edges x <-> z and y <-> z
    * This graph has the property that there are at least 2 distinct paths between any 2 nodes.
    */
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

  /**
    * Compute a sequence of coordinates connecting 2 rectangles.
    * - The corridor must start and end on a coordinate with odd x and y values.
    * - If possible, the corridor should be a straight line.
    * - Otherwise, it should be an L-shape connecting the midpoints of the 2 rectangles.
    * - The coordinates intersecting either of the 2 rectangles should be removed.
    */
  // TODO It is possible for the joint of an L-shaped corridor to be adjacent to a hub rectangle or corridor.
  private def computeCorridor(from: Rectangle, to: Rectangle): Corridor = {
    val xIntersection = (from.x until from.x + from.width).intersect(to.x until to.x + to.width).filter(_ % 2 == 1)
    val yIntersection = (from.y until from.y + from.height).intersect(to.y until to.y + to.height).filter(_ % 2 == 1)

    val coords =
      if(xIntersection.nonEmpty) {
        val x = xIntersection(xIntersection.length/2)
        Coord(x,from.midpoint.y).lineTo(Coord(x,to.midpoint.y))
      }
      else if(yIntersection.nonEmpty) {
        val y = yIntersection(yIntersection.length/2)
        Coord(from.midpoint.x,y).lineTo(Coord(to.midpoint.x,y))
      }
      else from.midpoint.ellTo(to.midpoint) // Hub rectangles are always centered on odd coordinates.

    coords.dropWhile(from.contains).takeWhile(!to.contains(_))
  }

  /**
    * Compute corridors that will map the graph onto a coordinate grid.
    * - All vertical corridors will lie along an odd x coordinate.
    * - All horizontal corridors will lie along an odd y coordinate.
    * These properties ensure that corridors will not run adjacent to each other.
    */
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
}
