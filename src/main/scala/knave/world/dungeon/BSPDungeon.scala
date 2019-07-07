package knave.world.dungeon

import scala.annotation.tailrec
import scala.util.Random

object BSPDungeon extends {
  import Dungeon.{height,width}
  import BSPDungeonGen._

  def apply(seed: Int): Dungeon = {

    implicit val rng = new Random(seed)

    val tileArray = Array.fill[Array[InnerTile]](width)(Array.fill[InnerTile](height)(new InnerWall))

    val initialPartitions = computePartitions

    val graph = computeGraph(initialPartitions)

    val corridors = computeCorridors(initialPartitions,graph)

    val finalPartitions = fusePartitionSequences(initialPartitions)

    val floors = corridors.flatten ++ finalPartitions.flatMap(_.fill)
    floors.foreach(c => tileArray(c.x)(c.y) = new PlainFloor)

    new InnerDungeon(tileArray,Nil,Map(),rng)
  }
}

private object BSPDungeonGen {
  import Dungeon.{width,height}

  case class Edge(from: Rectangle, to: Rectangle) {
    lazy val reverse = Edge(to,from)
  }

  type Corridor = Seq[Coord]

  def computePartitions(implicit rng: Random): Seq[Rectangle] = {
    val minWidth = 5
    val minHeight = 3

    def partitionHorizontally(rect: Rect): Seq[Rect] = {
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

    @tailrec
    def loop(parts: Seq[Rect]): Seq[Rect] = {
      val newParts = parts.flatMap(partitionHorizontally).flatMap(partitionVertically)
      if(newParts.length == parts.length) rng.shuffle(newParts).drop(newParts.length / 4)
      else loop(newParts)
    }
    loop(Seq(Rect(1,1,width-2,height-2)))
  }

  def computeGraph(rects: Seq[Rectangle]): Seq[Edge] = {
    @tailrec
    def loop(rects: Seq[Rectangle], edges: Seq[Edge]): Seq[Edge] = rects match {
      case r1 +: r2 +: r3 +: rs => loop(r2 +: r3 +: rs, Edge(r1,r3) +: Edge(r2,r3) +: edges)
      case r1 +: r2 +: Seq() => Edge(r1,r2) +: edges
      case _ => Nil
    }
    val rect1 +: rect2 +: _ = rects // There will always be 3 or more partitions so this is safe.
    loop(rects, Seq(Edge(rect1,rect2)))
  }

  // Corridors only run along odd rows and columns. This guarantees that they won't run adjacent to each other.
  private def computeCorridor(from: Rectangle, to: Rectangle): Corridor = {
    def makeOdd(c: Coord): Coord = {
      val newX = if(c.x % 2 == 0) c.x+1 else c.x
      val newY = if(c.y % 2 == 0) c.y+1 else c.y
      Coord(newX,newY)
    }

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
      else {
        val c1 = makeOdd(from.midpoint)
        val c2 = makeOdd(to.midpoint)
        c1.ellTo(c2)
      }

    coords.dropWhile(from.contains).takeWhile(!to.contains(_))
  }

  def computeCorridors(rects: Seq[Rectangle], graph: Seq[Edge]): Seq[Corridor] = {
    def isIntermediateRect(c: Coord, edge: Edge)(rect: Rectangle): Boolean = {
      val intersects = rect.contains(c)
      val adjacent = rect.adjacent(c)
      rect != edge.from && rect != edge.to && (intersects || adjacent)
    }

    @tailrec
    def loop(edges: Seq[Edge], considered: Set[Edge], processed: Set[Edge] = Set(), acc: Seq[Corridor] = Nil): Seq[Corridor] = edges match {
      case e +: es if processed.intersect(Set(e,e.reverse)).nonEmpty => loop(es,considered,processed,acc)
      case (e @ Edge(from,to)) +: es =>
        val corridor = computeCorridor(from,to)
        corridor.flatMap(c => rects.find(isIntermediateRect(c,e))).headOption match {
          case None => loop(es, considered + e + e.reverse, processed + e + e.reverse, corridor +: acc)
          case Some(rect) =>
            // Keeping track of which edges have been considered prevents infinite cycles.
            val newEdges = Seq(Edge(from,rect), Edge(rect,to)).filterNot(it => processed.contains(it) || considered.contains(it))
            loop(newEdges ++ es, considered ++ newEdges, processed, acc)
        }
      case _ => acc
    }
    loop(graph,graph.headOption.toSet)
  }

  def fusePartitionSequences(rects: Seq[Rectangle]): Seq[Rectangle] = {
    val rows = rects.groupBy(_.y).values
    val cohorts = rows.flatMap(_.groupBy(_.height).values).map(_.sortBy(_.x))

    @tailrec
    def findSequences(rects: Seq[Rectangle], sequences: Seq[Seq[Rectangle]] = Nil, singles: Seq[Rectangle] = Nil): (Seq[Seq[Rectangle]], Seq[Rectangle]) = rects match {
      case r1 +: r2 +: r3 +: rs =>
        val isSequence = (r1.x + r1.width + 1) == r2.x && (r2.x + r2.width + 1) == r3.x
        if(isSequence) findSequences(rs, Seq(r1,r2,r3) +: sequences, singles)
        else findSequences(r2 +: r3 +: rs, sequences, r1 +: singles)
      case rs => (sequences, rs ++ singles)
    }

    val (sequences, singles) = cohorts.map(findSequences(_)).foldLeft((Seq[Seq[Rectangle]](), Seq[Rectangle]()))((acc, it) => {
      val (sequences, singles) = acc
      val (newSequences, newSingles) = it
      (newSequences ++ sequences, newSingles ++ singles)
    })

    sequences.flatMap{ seq =>
      val Seq(r1,r2,r3) = seq
      val w = r1.width + r2.width + r3.width + 2
      if(w >= 18)
        Seq(Rect(r1.x,r1.y,w,r1.height))
      else seq
    } ++ singles
  }
}
