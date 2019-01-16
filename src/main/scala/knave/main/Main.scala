package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.game.Action
import knave.main.InputProcessor.{Look, Start}
import knave.world.World
import knave.world.dungeon.Coord

import scala.scalajs.js

sealed trait GameState
case object Ongoing extends GameState
case object Dead extends GameState
case object Ascended extends GameState

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = if(e.keyCode == 32) "space" else if (e.keyCode == 27) "escape" else e.key }

  var state : GameState = Ongoing

  val world = World.createRandomRoomsWorld(100)
  Display.display(world, List("Welcome to Knave...", "Use 'wasd' to move and space to look around."))
  js.timers.setInterval(10)({
    if(state == Ongoing) {
      val oldState = InputProcessor.state
      val actions = InputProcessor.process(world, input, Display.mousePos)
      input = ""
      if(actions.nonEmpty) {
        val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
        val logs = Action.applyActions(world, actions ++ enemyActions)
        Display.display(world, logs)
        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended)
          state = Ascended
      }
      else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) Display.display(world) else ()
        case Look => Display.displayLook(world)
        case InputProcessor.RayAttack(range, _, _) => Display.displayRayAttack(world, range)
        case _ => ()
      }
    }
  })
}
