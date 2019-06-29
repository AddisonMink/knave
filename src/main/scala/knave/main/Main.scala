package knave.main

import knave.display.DisplayDungeon
import org.scalajs.dom.document
import knave.game._
import knave.display.DisplayFov._
import knave.world.dungeon.HubDungeon

import scala.scalajs.js
import scala.util.Random

sealed trait GameState
case object Ongoing extends GameState
case object Dead extends GameState
case object Ascended extends GameState

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = if(e.keyCode == 32) "space" else if (e.keyCode == 27) "escape" else e.key }

  val seed = Random.nextInt
  println(seed)
  val game = new Game(seed)

  
  display(game.world, false, "Welcome to Knave. Use 'wasdqezc' and to move and 'space' to look around.")
  js.timers.setInterval(10)({
    game.run(input, mousePos)
    input = ""
  })
}
