package knave.dungeon

import scala.collection.mutable.ListBuffer

private sealed trait Shape {

   def fill : List[Coord]
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


