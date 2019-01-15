package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.game.Action
import knave.main.InputProcessor.{Look, Start}
import knave.world.World
import knave.world.dungeon.Coord

import scala.scalajs.js

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = if(e.keyCode == 32) "space" else if (e.keyCode == 27) "escape" else e.key }

  val world = World.createRandomRoomsWorld(100)
  Display.display(world, List("Welcome to Knave...", "Use 'wasd' to move and space to look around."))
  js.timers.setInterval(10)({
    val actions = InputProcessor.process(world, input)
    input = ""
    if(actions.nonEmpty) {
      val playerLogs = Action.applyActions(world, actions)
      val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
      val logs = Action.applyActions(world, enemyActions)
      Display.display(world, playerLogs ++ logs)
    }
    else InputProcessor.state match {
      case Start => ()
      case Look => {
        Display.displayLook(world)
      }
    }
  })
}
