package knave.world.dungeon

case class Coord(x : Int, y : Int) {

  lazy val inBounds : Boolean =
    x >= 0 && x < Size.width && y >= 0 && y < Size.height

  def lineTo(c : Coord) : Stream[Coord] = {
    val dy = c.y - this.y
    val dx = c.x - this.x
    val slope = if(dx != 0d) dy.toDouble / dx.toDouble else 0d
    val xdirection = (if (dx < 1) -1 else 1).toDouble
    val ydirection = (if (dy < 1) -1 else 1).toDouble

    def horizontalLoop(cur : Coord, acc : Double) : Stream[Coord] =
      if(cur == c) Stream(cur)
      else {
        val x = cur.x + xdirection.toInt
        if(x == c.x) Stream(cur,c)
        else {
          val newAcc = acc + xdirection*slope
          val y = Math.round(this.y.toDouble + newAcc).toInt
          Stream.cons(cur, horizontalLoop(Coord(x,y), newAcc))
        }
      }

    def verticalLoop(cur : Coord, acc : Double) : Stream[Coord] =
      if(cur == c) Stream(cur)
      else {
        val y = cur.y + ydirection.toInt
        if(y == c.y) Stream(cur,c)
        else {
          val newAcc = if(slope != 0) acc + ydirection*(1d/slope) else acc + 0d
          val x = Math.round(this.x.toDouble + newAcc).toInt
          Stream.cons(cur, verticalLoop(Coord(x,y), newAcc))
        }
      }

    if(Math.abs(dx) >= Math.abs(dy)) horizontalLoop(this, 0d).tail.takeWhile(_.inBounds)
    else verticalLoop(this, 0d).tail.takeWhile(_.inBounds)
  }

  def nextCoord(c: Coord) : Option[Coord] =
    this.lineTo(c).filter(_.inBounds).headOption

  lazy val adjacent : Stream[Coord] = Stream(
    Coord(x-1, y-1),
    Coord(x,y-1),
    Coord(x+1,y-1),
    Coord(x-1,y),
    Coord(x+1,y),
    Coord(x-1, y+1),
    Coord(x,y+1),
    Coord(x+1,y+1)
  )

  lazy val cardinalAdjacent : Stream[Coord] = Stream(
    Coord(x,y-1),
    Coord(x-1,y),
    Coord(x+1,y),
    Coord(x,y+1)
  )

  def distance(c : Coord) : Int = {
    val dx = (c.x - x).toFloat
    val dy = (c.y - y).toFloat
    Math.sqrt(dx*dx + dy*dy).toInt
  }

  def manhattanDistance(c : Coord) : Int = {
    val dx = Math.abs(c.x - x)
    val dy = Math.abs(c.y - y)
    dx + dy
  }

  def normalize : Coord =
    Coord(x / Math.abs(x), y / Math.abs(y))
}


