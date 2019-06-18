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

  def intersects(r: Rectangle): Boolean = {
    val intersectsHorizontally =
      (x >= r.x && x < r.x + r.width) || (r.x >= x && r.x < x + width)
    val intersectsVertically =
      (y >= r.y && y < r.y + r.height) || (r.y >= y && r.y < y + height)
    intersectsHorizontally && intersectsVertically
  }

  def cardinalAdjacent(r: Rectangle): Boolean = {
    lazy val left = x + width == r.x
    lazy val right = r.x + r.width == x
    lazy val verticalIntersect = (y >= r.y && y < r.y + r.height) || (r.y >= y && r.y < y + height)

    lazy val up = y + height == r.y
    lazy val down = r.y + r.height == y
    lazy val horizontalIntersect = (x >= r.x && x < r.x + r.width) || (r.x >= x && r.x < x + width)

    lazy val horizontalAdjacent = (left || right) && verticalIntersect
    lazy val verticalAdjacent = (up || down) && horizontalIntersect

    (horizontalAdjacent || verticalAdjacent) && !diagonalAdjacent(r)
  }

  def cardinalAdjacent(c: Coord): Boolean = {
    val rect = Rect(c.x,c.y,1,1)
    cardinalAdjacent(rect)
  }

  def diagonalAdjacent(r: Rectangle): Boolean = {
    lazy val left = x + width == r.x
    lazy val right = r.x + r.width == x
    lazy val up = y + height == r.y
    lazy val down = r.y + r.height == y
    left && up || left && down || right && up || right && down
  }

  def adjacent(r: Rectangle): Boolean = cardinalAdjacent(r) || diagonalAdjacent(r)

  def adjacent(c: Coord): Boolean = {
    val rect = Rect(c.x,c.y,1,1)
    adjacent(rect)
  }

  val area = width*height

  lazy val fill = for {
    cx <- x until x+width
    cy <- y until y+height
  } yield Coord(cx,cy)
}

case class Rect(x: Int, y: Int, width: Int, height: Int) extends Rectangle