package knave.world.dungeon

import Math.{abs, ceil, max}

case class Coord(x : Int, y : Int) {

  def +(cx: Int, cy: Int) = Coord(x+cx,y+cy)
  def +(tup: (Int,Int)) = Coord(x+tup._1, y+tup._2)

  lazy val adjacent = Seq(Coord(x-1, y-1), Coord(x,y-1), Coord(x+1,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x-1, y+1), Coord(x,y+1), Coord(x+1,y+1))

  lazy val cardinalAdjacent = Seq(Coord(x,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x,y+1))

  def distance(c: Coord): Int = max(abs(c.x - x), abs(c.y - y))

  def manhattanDistance(c : Coord): Int = abs(c.x - x) + abs(c.y - y)

  def ellTo(c: Coord): Seq[Coord] = {
    (c.x - x, c.y - y) match {
      case (0,0) => Nil
      case (dx,0) => Seq.range(x,c.x+dx.signum,dx.signum).map(Coord(_,y))
      case (0,dy) => Seq.range(y,c.y+dy.signum,dy.signum).map(Coord(x,_))
      case (dx,dy) =>
        val horizontal = Seq.range(x,c.x+dx.signum,dx.signum)
        val vertical = Seq.range(y,c.y+dy.signum,dy.signum)
        if(dx <= dy)
          horizontal.map(Coord(_,y)) ++ vertical.map(Coord(c.x,_))
        else
          vertical.map(Coord(x,_)) ++ horizontal.map(Coord(_,c.y))
    }
  }

  def lineTo(c: Coord): Seq[Coord] = {
    val slope = if(c.x == this.x) None else Some((c.y - this.y).toDouble / (c.x - this.x).toDouble)
    slope match {
      case _ if this == c => Nil

      case Some(slope) if abs(slope) <= 1 =>
        val dx = c.x - this.x
        val dir = dx.signum
        Seq.range(dir, dx+dir, dir).map(x => Coord(x,(slope*x.toDouble).toInt) + (this.x, this.y))

      case Some(slope) => // slope > 1
        val dy = c.y - this.y
        val dir = dy.signum
        Seq.range(dir, dy+dir, dir).map(y => Coord((y.toDouble/slope).toInt, y) + (this.x, this.y))

      case None =>
        val dy = c.y - this.y
        val dir = dy.signum
        Seq.range(dir, dy+dir, dir).map(y => Coord(0,y) + (this.x,this.y))
    }
  }

  def toward(c: Coord) : Option[Coord] =
    this.lineTo(c).headOption

  def cone(radius: Int, dir: Direction, predicate: Coord => Boolean = _ => true): Seq[Coord] = {
    val forward = ceil(radius.toDouble / 2.0).toInt
    val r = radius-1

    val (corner1, corner2, vertex) =
      dir match {
      case Horizontal(dx) => (this + (dx*radius,forward), this + (dx*radius,-forward), this + (dx*radius,0))
      case Vertical(dy) => (this + (forward,dy*r), this + (-forward,dy*r), this + (0,dy*r))
      case Diagonal(dx,dy) => (this + (dx*r,0), this + (0,dy*r), this + (dx*forward,dy*forward))
      case NoDirection(()) => (this,this,this)
    }

    val rim = corner1.lineTo(vertex) ++ vertex.lineTo(corner2) ++ corner2.lineTo(vertex) ++ vertex.lineTo(corner1)
    (for {
      r <- rim
      c <- this.lineTo(r).takeWhile(predicate)
    } yield c).distinct
  }

  def disk(radius: Int, predicate: Coord => Boolean = _ => true): List[Coord] =
    List(Direction(1,1), Direction(1,-1), Direction(-1,-1), Direction(-1,1)).flatMap(cone(radius,_,predicate))

}

class Direction(x: Int, y: Int) {
  val dx: Int = x.signum
  val dy: Int = y.signum
}

object Direction {
  def apply(c1: Coord, c2: Coord) = new Direction(c2.x - c1.x, c2.y - c1.y)
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