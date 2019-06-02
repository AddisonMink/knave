import knave.world.dungeon.{Coord, Direction}
import org.scalatest.{FlatSpec, Matchers}

class CoordTests extends FlatSpec with Matchers {

  private def printShape(cs: Seq[Coord]): Unit = {
    val result = cs.toSet
    for(y <- 0 until 50) {
      for(x <- 0 until 50)
        if(result.contains(Coord(x,y))) print('x') else print('.')
      println
      println
    }
  }

  {
    println("DIAGONAL DOWN-RIGHT CONE")
    val result = Coord(0,0).cone(8, Direction(1,1))
    printShape(result)
    println
  }

  {
    println("DIAGONAL UP-LEFT CONE")
    val result = Coord(9,9).cone(8, Direction(-1,-1))
    printShape(result)
    println
  }

  {
    println("CARDINAL RIGHT CONE")
    val result = Coord(0,5).cone(8, Direction(1,0))
    printShape(result)
    println
  }

  {
    println("CARDINAL UP CONE")
    val result = Coord(5,9).cone(8, Direction(0,-1))
    printShape(result)
    println
  }

  {
    println("DISK")
    val result = Coord(20,20).disk(16)
    printShape(result)
    println
  }
}
