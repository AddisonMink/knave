package knave.main

import org.scalajs.dom.document
import knave.game._
import knave.display.Display._
import scala.scalajs.js

sealed trait GameState
case object Ongoing extends GameState
case object Dead extends GameState
case object Ascended extends GameState


object Main extends App {

  var input = ""
  document.onkeydown = { e => input = if(e.keyCode == 32) "space" else if (e.keyCode == 27) "escape" else e.key }

  val seed = -451063954
  println(seed)
  val game = new Game(seed)

  
  display(game.world, false)
  js.timers.setInterval(10)({
    game.run(input)
    input = ""
  })
}
