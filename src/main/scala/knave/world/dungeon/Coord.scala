package knave.world.dungeon

import Math.{abs, sqrt}

import scala.annotation.tailrec

case class Coord(x : Int, y : Int) {

  // TODO This shouldn't be part of Coord. The members of the Size object should be part of the Dungeon trait, and Dungeon should do this calculation.
  lazy val inBounds : Boolean =
    x >= 0 && x < Size.width && y >= 0 && y < Size.height

  /**
    * Arithmetic functions for Coords.
    */
  def +(c: Coord): Coord = Coord(x+c.x, y+c.y)

  def +(cx: Int, cy: Int) = Coord(x+cx,y+cy)

  /**
    * The first coord in each cardinal and ordinal direction is defined to be adjacent to this.
    */
  lazy val adjacent = Seq(Coord(x-1, y-1), Coord(x,y-1), Coord(x+1,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x-1, y+1), Coord(x,y+1), Coord(x+1,y+1))

  /**
    * Get only the coords cardinally adjacent to this.
    */
  lazy val cardinalAdjacent = Seq(Coord(x,y-1), Coord(x-1,y), Coord(x+1,y), Coord(x,y+1))

  /**
    * Get Euclidean distance between this and c.
    */
  def distance(c: Coord): Int = sqrt((c.x - x)^2 + (c.y - y)^2).toInt

  /**
    * Get Manhattan distance between this and c. This is for use in dungeon generation.
    */
  def manhattanDistance(c : Coord): Int = abs(c.x - x) + abs(c.y - y)

  // TODO Get rid of this.
  lazy val normalize = Coord(x / Math.abs(x), y / Math.abs(y))

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

  /**
    * Compute only the first Coord on the line from this to c.
    */
  // TODO Dungeon should do bounds checking, not Coord.
  def nextCoord(c: Coord) : Option[Coord] =
    this.lineTo(c).find(_.inBounds)

  /**
    * Compute the coordinates within a given distance of this. The disk will look like a square because diagonals are considered adjacent.
    *
    * The disk is computed by computing lines from this to each coordinate of the circle's perimeter.
    * By default, lines will be computed with the lineTo method, but you can pass in your own method for computing lines.
    *
    * Because of the rounding that occurs in the lineTo method, larger circles may have holes in them. To avoid this, all circles of size 4 or greater
    * are computed by finding circles with radii 3,4,...,maxRadius and taking the union of the results.
    */

  // TODO Add filter
  @tailrec
  private def dsk(remaining: Int, topLeft: Option[Coord], top: Iterable[Coord], topRight: Option[Coord], left: Iterable[Coord], right: Iterable[Coord], botLeft: Option[Coord],
          bot: Iterable[Coord], botRight: Option[Coord], acc: Iterable[Coord]): Iterable[Coord] =
    if(remaining == 1) acc else {

    val newTopLeft = topLeft.map(_ + (-1,1))
    val newTopRight = topRight.map(_ + (1,1))
    val newBotLeft = botLeft.map(_ + (-1,-1))
    val newBotRight = botRight.map(_ + (1,-1))

    val newLeft = topLeft.map(_ + (-1,0)) ++ botLeft.map(_ + (-1,0)) ++ left.map(_ + (-1,0))
    val newRight = topRight.map(_ + (1,0)) ++ botRight.map(_ + (1,0)) ++ right.map(_ + (1,0))
    val newTop = topLeft.map(_ + (0,1)) ++ topRight.map(_ + (0,1)) ++ top.map(_ + (0,1))
    val newBot = botLeft.map(_ + (0,-1)) ++ botRight.map(_ + (0,-1)) ++ bot.map(_ + (0,-1))

    val newAcc = newTopLeft ++ newTopRight ++ newBotLeft ++ newBotRight ++ newTop ++ newBot ++ newLeft ++ newRight ++ acc
    dsk(remaining-1, newTopLeft, newTop, newTopRight, newLeft, newRight, newBotLeft, newBot, newBotRight, newAcc)
  }

  def dsk(radius: Int): List[Coord] = {
    val topLeft = Some(this + (-1,1))
    val top = Seq(this + (0,1))
    val topRight = Some(this + (1,1))
    val left = Seq(this + (-1,0))
    val right = Seq(this + (1,0))
    val botLeft = Some(this + (-1,-1))
    val bot = Seq(this + (0,-1))
    val botRight = Some(this + (1,-1))
    val acc = topLeft ++ top ++ topRight ++ left ++ right ++ botLeft ++ bot ++ botRight
    dsk(radius, topLeft, top, topRight, left, right, botLeft, bot, botRight, acc).toList
  }


  // TODO This blows the stack on radius of 10 or greater.
  def disk(radius: Int, computeLine: (Coord,Coord) => Stream[Coord]): Stream[Coord] = (for {
    //radius <- if(maxRadius < 4) Stream(maxRadius) else Stream.range(3,maxRadius+1)
    bot <- Coord(x-radius,y-radius).lineTo(Coord(x+radius,y-radius))
    right <- Coord(x+radius,y-radius).lineTo(Coord(x+radius,y+radius))
    top <- Coord(x+radius,y+radius).lineTo(Coord(x-radius,y+radius))
    left <- Coord(x-radius,y+radius).lineTo(Coord(x-radius,y-radius))
    perimeter <- Seq(bot,right,top,left)
    ray <- computeLine(this,perimeter)
  } yield ray).distinct

  def disk(maxRadius: Int): Stream[Coord] = disk(maxRadius, (c1,c2) => c1.lineTo(c2))


  /**
    * Compute a cone of a given length extending toward a given Coord. The radius of the cone will be length / 2 + 1.
    *
    * Because of the distortion caused by making diagonals adjacent, ordinal-facing cones will look like squares with 1 corner on this.
    *
    * The cone is computed by finding the outer edge of the cone and using the given "computeLine" function to find the union of all lines
    * from this to each Coord in the edge. By default, "lineTo" is used, but you can pass in your own function.
    *
    * Because of the rounding that occurs in the "lineTo" method, large cones may bave holes in them. To avoid this, all cones with length 4
    * or greater are computed by finding the cones with lengths, 3,4,...,maxLength and taking the union of the result.
    *
    *            x
    *      x x x x
    *  x x x x x x
    *  _ x x x x x
    *  x x x x x x
    *    x x x x x
    *            x
    *
    *  cardinal cone with length 5
    *
    *  _ x x x x x
    *  x x x x x x
    *  x x x x x x
    *  x x x x x x
    *  x x x x x x
    *  x x x x x x
    *
    *  ordinal cone with length 5
    */
  def cone(length: Int, towards: Coord, computeLine: (Coord,Coord) => Stream[Coord]): Stream[Coord] = ((towards.x - x).signum, (towards.y - y).signum) match {
    case (0,0) => Stream()

    case (x,y) if x == 0 || y == 0 => cardinalCone(length, x, y, computeLine)

    case (x,y) => ordinalCone(length, x, y, computeLine)
  }

  def cone(length: Int, towards: Coord): Stream[Coord] = cone(length, towards, (c1, c2) => c1.lineTo(c2))

  private def cardinalCone(maxlength: Int, xdir: Int, ydir: Int, computeLine: (Coord,Coord) => Stream[Coord]): Stream[Coord] = (for {
    length <- if(maxlength < 4) Stream(maxlength) else Stream.range(3,maxlength+1)
    width = length/2 + 1
    edge <- (xdir.signum,ydir.signum) match {
      case (dir,0) =>
        val top = Coord(x + dir*length, y+width)
        top +: top.lineTo(Coord(top.x, y+width))
      case (0,dir) =>
        val right = Coord(x+width, y + dir*length)
        right +: right.lineTo(Coord(x-width, right.y))
      case _ => Stream()
    }
    ray <- computeLine(this,edge)
  } yield ray).distinct

  private def ordinalCone(maxRadius: Int, xdir: Int, ydir: Int, computeLine: (Coord,Coord) => Stream[Coord]): Stream[Coord] = (for {
    radius <- if(maxRadius < 4) Stream(maxRadius) else Stream.range(3,maxRadius+1)
    corner = Coord(x + xdir.signum*radius, y + ydir.signum*radius)
    xCorner = Coord(x + xdir.signum*radius, y)
    yCorner = Coord(x, y + ydir.signum*radius)
    edge <- xCorner +: (xCorner.lineTo(corner) ++ corner.lineTo(yCorner))
    ray <- computeLine(this,edge)
  } yield ray).distinct
}