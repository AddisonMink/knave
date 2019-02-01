package knave.main

import java.util.Calendar

import org.scalajs.dom.document
import knave.display.DisplayFull
import knave.game._
import knave.main.InputProcessor.{LogMore, Look, LookMore, Start}
import knave.world.World
import knave.world.dungeon.Dungeon
import knave.display.DisplayFull._

import scala.scalajs.js
import scala.util.Random

sealed trait GameState
case object Ongoing extends GameState
case object Dead extends GameState
case object Ascended extends GameState

object Main extends App {

  var input = ""
  document.onkeydown = { e => input = if(e.keyCode == 32) "space" else if (e.keyCode == 27) "escape" else e.key }

  var state : GameState = Ongoing

  var round = 1

  val seed = Random.nextInt
  val world = World.standardWorld(Dungeon.hubDungeon(seed))
  display(world,List(),false)
  js.timers.setInterval(10)({
    if(input.nonEmpty) {
      input = ""
      val actions = world.getEnemies.flatMap(_.act(world)).toVector
      Action.applyActions(world,actions)
      display(world,List(),round % 3 == 0)
      round += 1
    }
  })


  /*
  Display.display(world, List("Welcome to Knave...", "Use 'wasdqezc' and to move and 'space' to look around."), false)
  js.timers.setInterval(10)({
    if(state == Ongoing) {
      val oldState = InputProcessor.state
      val playerActions = InputProcessor.process(world, input, Display.mousePos)
      input = ""
      if(playerActions.nonEmpty) {
        if(round % 3 == 0) {
          val enemyActions = world.getEnemies.filter(_.speed != Slow).flatMap(_.act(world)).toVector
          val logs = Action.applyActions(world, playerActions ++ enemyActions)

          val fastActions = world.getEnemies.filter(_.speed == Fast).flatMap(_.act(world)).toVector
          val fastLogs = Action.applyActions(world, fastActions)

          val stealthActions =
            if(world.player.hidden && world.getEnemies.exists(_.canSeePlayer(world))) {
              for(e <- world.getEnemies)
                e.onAlert
              Vector(SpotSplayer)
            }
            else if(!world.player.hidden && !world.getEnemies.exists(_.canSeePlayer(world))) {
              for(e <- world.getEnemies)
                e.onHidden
              Vector(HidePlayer)
            }
            else Vector()
          val stealthLogs = Action.applyActions(world, stealthActions)

          Display.display(world, logs ++ fastLogs ++ stealthLogs, (round + 1) % 3 == 0)
        }
        else {
          val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
          val logs = Action.applyActions(world, playerActions ++ enemyActions)

          val stealthActions =
            if(world.player.hidden && world.getEnemies.exists(_.canSeePlayer(world))) {
              for(e <- world.getEnemies)
                e.onAlert
              Vector(SpotSplayer)
            }
            else if(!world.player.hidden && !world.getEnemies.exists(_.canSeePlayer(world))) {
              for(e <- world.getEnemies)
                e.onHidden
              Vector(HidePlayer)
            }
            else Vector()
          val stealthLogs = Action.applyActions(world, stealthActions)

          Display.display(world, logs ++ stealthLogs, (round + 1) % 3 == 0)
        }


        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended)
          state = Ascended

        round += 1
      }
      else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) Display.display(world, List(), round % 3 == 0)
        case Look => Display.displayLook(world, InputProcessor.state == oldState, round % 3 == 0)
        case InputProcessor.RayAttack(range, _, _) => Display.displayRayAttack(world, range, InputProcessor.state == oldState, round % 3 == 0)
        case LogMore => if(InputProcessor.state != oldState) Display.displayLogMore
        case LookMore => if(InputProcessor.state != oldState) Display.displayLookMore(world)
        case _ => ()
      }
    }
  })
  */
}
