import knave.world.dungeon.Coord
import org.scalatest.{FlatSpec, Matchers}

class CoordTests extends FlatSpec with Matchers {

  /**
    * I'm only unit testing the lineTo method. All the other Coord methods are trivial.
    */



  behavior of "Coord"

  it should "compute a line from left to right where rise is greater than run." in {
    val result = Coord(5,5).lineTo(Coord(7,10)).toList
    result should contain theSameElementsInOrderAs Seq(Coord(5,6), Coord(5,7), Coord(6,8), Coord(6,9), Coord(7,10))
  }

  it should "compute a line from left to right where run is greater than rise." in {
    val result = Coord(5,5).lineTo(Coord(10,7)).toList
    result should contain theSameElementsInOrderAs Seq(Coord(6,5), Coord(7,5), Coord(8,6), Coord(9,6), Coord(10,7))
  }

  it should "compute a line from right to left where rise is greater than run." in {
    val result = Coord(7,10).lineTo(Coord(5,5)).toList
    result should contain theSameElementsInOrderAs Seq(Coord(7,9), Coord(7,8), Coord(6,7), Coord(6,6), Coord(5,5))
  }

  it should "compute a line from right to left where run is greater than rise." in {
    val result = Coord(10,7).lineTo(Coord(5,5)).toList
    result should contain theSameElementsInOrderAs Seq(Coord(9,7), Coord(8,7), Coord(7,6), Coord(6,6), Coord(5,5))
  }

  it should "compute a horizontal line from left to right." in {
    val result = Coord(5,5).lineTo(Coord(10,5))
    result should contain theSameElementsInOrderAs Seq(Coord(6,5),Coord(7,5),Coord(8,5),Coord(9,5),Coord(10,5))
  }

  it should "compute a horizontal line from right to left." in {
    val result = Coord(10,5).lineTo(Coord(5,5))
    result should contain theSameElementsInOrderAs Seq(Coord(9,5),Coord(8,5),Coord(7,5),Coord(6,5),Coord(5,5))
  }

  it should "compute a vertical line from down to up." in {
    val result = Coord(5,5).lineTo(Coord(5,10))
    result should contain theSameElementsInOrderAs Seq(Coord(5,6),Coord(5,7),Coord(5,8),Coord(5,9),Coord(5,10))
  }

  it should "compute a vertical line from up to down." in {
    val result = Coord(5,10).lineTo(Coord(5,5))
    result should contain theSameElementsInOrderAs Seq(Coord(5,9),Coord(5,8),Coord(5,7),Coord(5,6),Coord(5,5))
  }
}
