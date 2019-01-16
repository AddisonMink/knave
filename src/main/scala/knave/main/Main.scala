package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.game.{Action, HidePlayer, SpotSplayer}
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
      val playerActions = InputProcessor.process(world, input, Display.mousePos)
      input = ""
      if(playerActions.nonEmpty) {
        val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
        val logs = Action.applyActions(world, playerActions ++ enemyActions)

        val stealthActions =
          if(world.player.hidden && world.getEnemies.exists(_.canSeePlayer(world))) Vector(SpotSplayer)
          else if(!world.player.hidden && !world.getEnemies.exists(_.canSeePlayer(world))) Vector(HidePlayer)
          else Vector()
        val stealthLogs = Action.applyActions(world, stealthActions)

        Display.display(world, logs ++ stealthLogs)
        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended)
          state = Ascended
      }
      else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) Display.display(world) else ()
        case Look => Display.displayLook(world, InputProcessor.state == oldState)
        case InputProcessor.RayAttack(range, _, _) => Display.displayRayAttack(world, range, InputProcessor.state == oldState)
        case _ => ()
      }
    }
  })
}
