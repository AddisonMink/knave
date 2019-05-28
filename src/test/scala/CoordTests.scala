import knave.world.dungeon.Coord
import org.scalatest.{FlatSpec, Matchers}

class CoordTests extends FlatSpec with Matchers {

  behavior of "Coord lineTo"

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

  behavior of "Coord disk"

  it should "compute a complete disk" in {
    val c = Coord(5,5)
    val radius = 100
    val disk = c.dsk(radius)
    val area = (radius*2+1)*(radius*2+1) - 1
    disk.forall(_.distance(c) <= radius) should be (true)
    disk should have length area
  }

  behavior of "Coord cone"

  it should "compute a horizontal cardinal cone."

  it should "compute a vertical cardinal cone."

  ignore should "compute an ordinal cone." in {
    val c = Coord(0,0)
    val towards = Coord(15,15)
    val cone = c.cone(10,towards)

    val expectedContents = (for {
      x <- 0 to 11
      y <- 0 to 11
    } yield Coord(x,y)).toList.filter(_ != c)

    cone should contain theSameElementsAs expectedContents
  }
}
