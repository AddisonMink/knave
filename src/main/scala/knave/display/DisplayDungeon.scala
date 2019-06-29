package knave.display

import knave.world.{Log, World}
import knave.world.dungeon.{Coord, Dungeon}

object DisplayDungeon extends Display {
  import Dungeon.{height,width}

  private def setDungeon(d : Dungeon) : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        setTile(d, Coord(x,y),true)

  override def display(w : World, speedRound : Boolean = false, prompt: String = "") : Unit = {
    setDungeon(w.dungeon)
  }

  def display(dungeon: Dungeon): Unit = {
    setDungeon(dungeon)
  }
}
