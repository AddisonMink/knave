package knave.display

import knave.world.{World}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import knave.world.dungeon.Size.{height, width}
import knave.world.enemy.Enemy
import Palette._

object DisplayDungeon extends Display {

  private def setDungeon(d : Dungeon) : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        setTile(d, Coord(x,y),true)

  override def display(w : World, logs : List[String] = List(), speedRound : Boolean) : Unit = {
    setDungeon(w.dungeon)
  }

  def display(dungeon: Dungeon): Unit = {
    setDungeon(dungeon)
  }
}
