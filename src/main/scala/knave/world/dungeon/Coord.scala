package knave.world.dungeon

import Math.{abs, ceil, max}

case class Coord(x : Int, y : Int) {

  // TODO This shouldn't be part of Coord. The members of the Size object should be part of the Dungeon trait, and Dungeon should do this calculation.
  lazy val inBounds : Boolean =
    x >= 0 && x < Size.width && y >= 0 && y < Size.height

  /**
    * Arithmetic functions for Coords.
    */
  def +(c: Coord): Coord = Coord(x+c.x, y+c.y)

  def +(cx: Int, cy: Int) = Coord(x+cx,y+cy)

  lazy val adjacent = Seq(Coord(x-1, y-1), Coord(x,y-1), Coord(x+1,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x-1, y+1), Coord(x,y+1), Coord(x+1,y+1))

  lazy val cardinalAdjacent = Seq(Coord(x,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x,y+1))

  def distance(c: Coord): Int = max(abs(c.x - x), abs(c.y - x))

  def manhattanDistance(c : Coord): Int = abs(c.x - x) + abs(c.y - y)

  // TODO Get rid of this.
  lazy val normalize = Coord(x / Math.abs(x), y / Math.abs(y))

  /**
    * Compute the line from this to c in R2 and project the line onto a grid (Z2). Excludes this.
    * Because of the precision loss during rounding, a.lineTo(b) is not necessarily the reverse of b.lineTo(a).
    */
  private def computeSlope(c: Coord): Option[Double] = if(c.x == this.x) None else Some((c.y - this.y).toDouble / (c.x - this.x).toDouble)

  def lineTo(c: Coord): Stream[Coord] = computeSlope(c) match {
    case Some(slope) if abs(slope) <= 1 =>
      val dx = c.x - this.x
      val dir = dx.signum
      Stream.range(dir, dx+dir, dir).map(x => Coord(x,(slope*x.toDouble).toInt) + (this.x, this.y))

    case Some(slope) => // slope > 1
      val dy = c.y - this.y
      val dir = dy.signum
      Stream.range(dir, dy+dir, dir).map(y => Coord((y.toDouble/slope).toInt, y) + (this.x, this.y))

    case None =>
      val dir = (c.y - this.y).signum
      Stream.range(this.y, c.y+dir, dir).map(Coord(this.x,_))
  }

  /**
    * Compute only the first Coord on the line from this to c.
    */
  // TODO Dungeon should do bounds checking, not Coord.
  def nextCoord(c: Coord) : Option[Coord] =
    this.lineTo(c).find(_.inBounds)

  def cone(radius: Int, dir: Direction): List[Coord] = {
    val start = this + (dir.dx, dir.dy)
    val diag = ceil(radius.toDouble / 2.0).toInt
    val r = radius-1

    val (corner1, corner2, vertex) =
      dir match {
      case Horizontal(dx) => (start + (dx*r,diag), start + (dx*r,-diag), start + (dx*r,0))
      case Vertical(dy) => (start + (diag,dy*r), start + (-diag,dy*r), start + (0,dy*r))
      case Diagonal(dx,dy) => (start + (dx*r,0), start + (0,dy*r), start + (dx*diag,dy*diag))
      case NoDirection(()) => (this,this,this)
    }

    val rim = corner1.lineTo(vertex) ++ vertex.lineTo(corner2) ++ corner2.lineTo(vertex) ++ vertex.lineTo(corner1)
    rim.flatMap(start.lineTo(_)).distinct.toList
  }

  def disk(radius: Int): List[Coord] = {
    val diagonals = List(Direction(1,1), Direction(1,-1), Direction(-1,-1), Direction(-1,1)).flatMap(cone(radius,_))
    val straights = List(Coord(x+radius,y), Coord(x-radius,y), Coord(x,y+radius), Coord(x,y-radius)).flatMap(lineTo)
    straights ++ diagonals
  }

}

class Direction(x: Int, y: Int) {
  val dx: Int = x.signum
  val dy: Int = y.signum
}

object Direction {
  def apply(c1: Coord, c2: Coord) = new Direction((c2.x - c1.x), (c2.y - c1.y))
  def apply(x: Int, y: Int) = new Direction(x,y)
}

object Horizontal {
  def unapply(dir: Direction): Option[Int] = if(dir.dy == 0 && dir.dx != 0) Some(dir.dx) else None
}

object Vertical {
  def unapply(dir: Direction): Option[Int] = if(dir.dx == 0 && dir.dy != 0) Some(dir.dy) else None
}

object Diagonal {
  def unapply(dir: Direction): Option[(Int,Int)] = if(dir.dx != 0 && dir.dy != 0) Some(dir.dx, dir.dy) else None
}

object NoDirection {
  def unapply(dir: Direction): Option[Unit] = if(dir.dx == 0 && dir.dy == 0) Some(()) else None
}