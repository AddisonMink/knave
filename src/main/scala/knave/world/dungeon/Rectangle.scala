package knave.world.dungeon

import scala.util.Random

trait Rectangle {

  val x: Int
  val y: Int
  val width: Int
  val height: Int

  def midpoint: Coord = {
    val mx = x + width/2
    val my = y + height/2
    Coord(mx,my)
  }

  def randomCoord(implicit rng : Random) : Coord =
    Coord(rng.nextInt(width) + x, rng.nextInt(height) + y)

  def contains(c : Coord) : Boolean =
    c.x >= x && c.x < x + width && c.y >= y && c.y < y + height

  def xOverlap(r: Rectangle): Int = {
    if(x >= r.x && x < r.x + r.width) Math.min((r.x + r.width) - x, width)
    else if(r.x >= x && r.x < x + width) Math.max((x + width) - r.x, r.width)
    else 0
  }

  def yOverlap(r: Rectangle): Int = {
    if(y >= r.y && y < r.x + r.height) Math.min((r.y + r.height) - y, height)
    else if(r.y >= y && r.y < y + height) Math.min((y + height) - r.y, r.height)
    else 0
  }

  def intersects(r: Rectangle): Boolean = {
    xOverlap(r) > 0 && yOverlap(r) > 0
  }

  def cardinalAdjacent(r: Rectangle): Boolean = {
    val aboveOrBelow = xOverlap(r) > 0 && (y + height == r.y || r.y + r.height == y)
    val leftOrRight = yOverlap(r) > 0 && (x + width == r.x || r.x + r.width == x)
    aboveOrBelow || leftOrRight
  }

  def diagonalAdjacent(r: Rectangle): Boolean = {
    val left = x + width == r.x
    val right = r.x + r.width == x
    val up = y + height == r.y
    val down = r.y + r.height == y
    left && up || left && down || right && up || right && down
  }

  val area = width*height

  lazy val fill = for {
    cx <- x until x+width
    cy <- y until y+height
  } yield Coord(cx,cy)
}

case class Rect(x: Int, y: Int, width: Int, height: Int) extends Rectangle