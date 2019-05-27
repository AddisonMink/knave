package knave.world.dungeon

import Math.{abs, sqrt}

case class Coord(x : Int, y : Int) {

  // TODO This shouldn't be part of Coord. The members of the Size object should be part of the Dungeon trait, and Dungeon should do this calculation.
  lazy val inBounds : Boolean =
    x >= 0 && x < Size.width && y >= 0 && y < Size.height

  /**
    * Compute the line from this to c in R2 and project the line onto a grid (Z2). Excludes this.
    * Because of the precision loss during rounding, a.lineTo(b) is not necessarily the reverse of b.lineTo(a).
    */
  private def computeSlope(c: Coord): Option[Double] = if(c.x == this.x) None else Some((c.y - this.y).toDouble / (c.x - this.x).toDouble)

  def lineTo(c: Coord): Stream[Coord] = computeSlope(c) match {
    case Some(slope) if slope <= 1 =>
      val dx = c.x - this.x
      Stream.range(dx.signum, dx+dx.signum, dx.signum).map(x => Coord(x+this.x, (x.toDouble*slope).toInt + this.y))

    case Some(slope) => // slope > 1
      val dy = c.y - this.y
      Stream.range(dy.signum, dy+dy.signum, dy.signum).map(y => Coord((y.toDouble/slope).toInt + this.x, y+this.y))

    case None =>
      val dir = (c.y - this.y).signum
      Stream.range(this.y+dir, c.y+dir, dir).map(Coord(this.x,_))
  }

  // TODO Dungeon should do bounds checking, not Coord.
  def nextCoord(c: Coord) : Option[Coord] =
    this.lineTo(c).find(_.inBounds)

  lazy val adjacent = Stream(Coord(x-1, y-1), Coord(x,y-1), Coord(x+1,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x-1, y+1), Coord(x,y+1), Coord(x+1,y+1))

  lazy val cardinalAdjacent = Stream(Coord(x,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x,y+1))

  def distance(c: Coord): Int = sqrt((c.x - x)^2 + (c.y - y)^2).toInt

  def manhattanDistance(c : Coord): Int = abs(c.x - x) + abs(c.y - y)

  lazy val normalize = Coord(x / Math.abs(x), y / Math.abs(y))
}