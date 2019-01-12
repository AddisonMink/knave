package knave.main

import knave.display.Display
import knave.dungeon.Dungeon

object Main extends App {
  val dungeon = Dungeon.createRandomRoomsDungeon(100)
  Display.display(dungeon)
}
