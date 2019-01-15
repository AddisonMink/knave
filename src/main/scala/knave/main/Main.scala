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
    val oldState = InputProcessor.state
    val actions = InputProcessor.process(world, input, Display.mousePos)
    input = ""
    if(actions.nonEmpty) {
      val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
      val logs = Action.applyActions(world, actions ++ enemyActions)
      Display.display(world, logs)
    }
    else InputProcessor.state match {
      case Start => if (InputProcessor.state != oldState) Display.display(world) else ()
      case Look => Display.displayLook(world)
      case InputProcessor.RayAttack(range, _, _) => Display.displayRayAttack(world, range)
      case _ => ()
    }
  })
}
