package knave.world.dungeon
import scala.util.Random

trait Room {

  def randomCoord(implicit rng : Random) : Coord

  final def randomCoordExcept(cs : Seq[Coord])(implicit rng : Random) : Option[Coord] = {
    var tries = 100
    var c = randomCoord
    while(tries > 0 && cs.contains(c))
      c = randomCoord
      tries += 1
    if(cs.contains(c)) None else Some(c)
  }

  def area : Int

  def contents : Iterable[Coord]
}

private case class ShapeRoom(shapes : List[Shape]) extends Room {

  def randomCoord(implicit rng: Random): Coord = {
    val i = rng.nextInt(shapes.length)
    shapes(i).randomCoord
  }

  override def area: Int = shapes.map(_.area).sum

  override def contents: Iterable[Coord] = shapes.flatMap(_.fill)
}

private case class SetRoom(coords : Set[Coord]) extends Room {

  private val coordVector = coords.toVector

  override def randomCoord(implicit rng: Random): Coord = {
    val i = rng.nextInt(coordVector.length)
    coordVector(i)
  }

  override def area: Int = coords.size

  override def contents: Iterable[Coord] = coords
}