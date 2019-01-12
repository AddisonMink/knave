package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.world.World
import org.scalajs.dom.raw.KeyboardEvent

object Main extends App {

  var input = ""
  document.onkeydown = (e : KeyboardEvent) => input = e.key ; println(input)

  val world = World.createDefaultWorld
  Display.display(world)
}
