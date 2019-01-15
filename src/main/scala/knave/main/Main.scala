package knave.main

import org.scalajs.dom.document
import knave.display.Display
import knave.game.Action
import knave.world.World
import knave.world.dungeon.Coord

import scala.scalajs.js

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = e.key }

  var mouse : Coord = Coord(0,0)
  Display.map.onmousemove = { e => {
    val x = e.pageX - Display.map.offsetLeft
    val y = e.pageY - Display.map.offsetTop
    mouse = Display.normalize(x.toInt, y.toInt)
    println(mouse)
  }}

  val world = World.createRandomRoomsWorld(100)
  Display.display(world)
  js.timers.setInterval(10)({
    val actions = InputProcessor.process(world, input)
    input = ""
    if(actions.nonEmpty) {
      val playerLogs = Action.applyActions(world, actions)
      val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
      val logs = Action.applyActions(world, enemyActions)
      Display.display(world, playerLogs ++ logs)
    }
  })
}
