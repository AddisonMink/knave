package knave.world.dungeon

import scala.collection.mutable.ListBuffer

private sealed trait Shape {

   def fill : List[Coord]
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
}

private case class Rectangle(x : Int, y : Int, width : Int, height : Int) extends Shape {

  lazy val fill = {
    val cs = new ListBuffer[Coord]
    for(x <- x until (x + width))
      for(y <- y until (y + height))
        cs += Coord(x,y)
    cs.toList
  }
}




