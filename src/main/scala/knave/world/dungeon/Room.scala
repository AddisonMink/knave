package knave.world.dungeon
import scala.collection.mutable.ListBuffer
import scala.util.Random

trait Room {

  def randomCoord(rng : Random) : Coord

  final def randomCoordExcept(cs : Seq[Coord], rng : Random) : Option[Coord] = {
    var tries = 100
    var c = randomCoord(rng)
    while(tries > 0 && cs.contains(c))
      c = randomCoord(rng)
    if(cs.contains(c)) None else Some(c)
  }

  def area : Int

  def contents : Iterable[Coord]
}

private case class ShapeRoom(shapes : List[Shape]) extends Room {

  override def randomCoord(rng: Random): Coord = {
    val i = rng.nextInt(shapes.length)
    shapes(i).randomCoord(rng)
  }

  override def area: Int = shapes.map(_.area).sum

  override def contents: Iterable[Coord] = shapes.flatMap(_.fill)
}

private case class SetRoom(coords : Set[Coord]) extends Room {

  private val coordVector = coords.toVector

  override def randomCoord(rng: Random): Coord = {
    val i = rng.nextInt(coordVector.length)
    coordVector(i)
  }

  override def area: Int = coords.size

  override def contents: Iterable[Coord] = coords
}

private sealed trait Shape {

  def fill : List[Coord]

  def randomCoord(rng : Random) : Coord

  def area : Int
}

private object Shape {

  def intersects(s1 : Shape, s2 : Shape) : Boolean =
    (s1, s2) match {
      case (r1 : Rectangle, r2 : Rectangle) => {
        lazy val intersectsHorizontally =
          (r1.x >= r2.x && r1.x < r2.x + r2.width) || (r2.x >= r1.x && r2.x < r1.x + r1.width)
        lazy val intersectsVertically =
          (r1.y >= r2.y && r1.y < r2.y + r2.height) || (r2.y >= r1.y && r2.y < r1.y + r1.height)
        intersectsHorizontally && intersectsVertically
      }
    }

  def adjacent(s1 : Shape, s2 : Shape) : Boolean =
    (s1, s2) match {
      case (r1 : Rectangle, r2 : Rectangle) => {
        lazy val left = r1.x + r1.width == r2.x
        lazy val right = r2.x + r2.width == r1.x
        lazy val verticalIntersect = (r1.y >= r2.y && r1.y < r2.y + r2.height) || (r2.y >= r1.y && r2.y < r1.y + r1.height)

        lazy val up = r1.y + r1.height == r2.y
        lazy val down = r2.y + r2.height == r1.y
        lazy val horizontalIntersect = (r1.x >= r2.x && r1.x < r2.x + r2.width) || (r2.x >= r1.x && r2.x < r1.x + r1.width)

        lazy val horizontalAdjacent = (left || right) && verticalIntersect
        lazy val verticalAdjacent = (up || down) && horizontalIntersect
        lazy val diagonalAdjacent = left && up || left && down || right && up || right && down

        (horizontalAdjacent || verticalAdjacent) && !diagonalAdjacent
      }
    }

  def diagonalAdjacent(s1 : Shape, s2 : Shape) : Boolean =
    (s1,s2) match {
      case (r1 : Rectangle, r2 : Rectangle) => {
        lazy val left = r1.x + r1.width == r2.x
        lazy val right = r2.x + r2.width == r1.x
        lazy val up = r1.y + r1.height == r2.y
        lazy val down = r2.y + r2.height == r1.y
        left && up || left && down || right && up || right && down
      }
    }
}

private case class Rectangle(x : Int, y : Int, width : Int, height : Int) extends Shape {

  lazy val fill = {
    val cs = new ListBuffer[Coord]
    for(x <- x until (x + width))
      for(y <- y until (y + height))
        cs += Coord(x,y)
    cs.toList
  }

  def randomCoord(rng : Random) : Coord =
    Coord(rng.nextInt(width) + x, rng.nextInt(height) + y)

  def contains(c : Coord) : Boolean =
     c.x >= x && c.x < x + width && c.y >= y && c.y < y + height

  override def area: Int = width*height
}




