package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.game.Action
import knave.world.World

import scala.scalajs.js

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = e.key }

  val world = World.createRandomRoomsWorld(100)
  Display.display(world)
  js.timers.setInterval(10)({
    val actions = InputProcessor.process(world, input)
    input = ""
    if(actions.nonEmpty) {
      Action.applyActions(world, actions)
      val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
      Action.applyActions(world, enemyActions)
      Display.display(world)
    }
  })
}
