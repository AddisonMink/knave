package knave.world.dungeon

package object Size {
  val height = 20
  val width = 78
}

trait Dungeon {

  def isFloor(c : Coord) : Boolean
  def floorAt(c : Coord) : Option[Floor]

  def isWall(c : Coord) : Boolean
  def wallAt(c : Coord) : Option[Wall]

  def isDoor(c : Coord) : Boolean
  def doorAt(c : Coord) : Option[Door]

  def isWalkable(c : Coord) : Boolean =
    isFloor(c) || doorAt(c).map(_.open).getOrElse(false)
}

object Dungeon {

  def createDefaultDungeon : Dungeon = new DefaultDungeon

  def createRandomRoomsDungeon(seed : Int) : Dungeon = new RandomRoomsDungeon(seed)
}
