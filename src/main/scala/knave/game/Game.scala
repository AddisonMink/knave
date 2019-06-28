package knave.game

import knave.display.DisplayFov._
import InputProcessor._
import knave.main._
import knave.world.World
import knave.world.dungeon.Coord

import scala.util.Random

class Game(seed : Int = Random.nextInt) {

  println(seed)
  var world = World(seed)

  private var state : GameState = Ongoing

  private var round = 1

  def run(input : String, mousePos : Coord): Unit = {
    if(state == Ongoing) {
      val oldState = InputProcessor.state
      val playerActions = InputProcessor.process(world, input, mousePos)
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

          display(world, logs ++ fastLogs ++ stealthLogs, (round + 1) % 3 == 0)
        }
        else {
          val enemyActions = world.getEnemies.flatMap(_.act(world)).toVector
          val logs = Action.applyActions(world, playerActions ++ enemyActions)

          val stealthActions =
            if(world.player.hidden && world.getEnemies.exists(_.canSeePlayer(world)))
              world.getEnemies.flatMap(_.onAlert).toVector :+ SpotSplayer
            else if(!world.player.hidden && !world.getEnemies.exists(_.canSeePlayer(world)))
              world.getEnemies.flatMap(_.onHidden).toVector :+ HidePlayer
            else Vector[Action]()

          val stealthLogs = Action.applyActions(world, stealthActions)

          display(world, logs ++ stealthLogs, (round + 1) % 3 == 0)
        }


        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended && world.depth < 5) {
          world = world.nextLevel
          display(world, List(), (round + 1) % 3 == 0)
        }
        else if(world.player.ascended && world.depth == 5) {
          display(world, List("You win!"), false)
          state = Ascended
        }

        round += 1
      }
      else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) display(world, List(), round % 3 == 0)
        case Look => displayLook(world, InputProcessor.state == oldState, round % 3 == 0)
        case InputProcessor.RayAttack(range, _, _) => displayRayAttack(world, range, InputProcessor.state == oldState, round % 3 == 0)
        case LogMore => if(InputProcessor.state != oldState) displayLogMore
        case LookMore => displayLookMore(world, InputProcessor.state == oldState)
        case Drop => if (InputProcessor.state != oldState) display(world, List("Which item do you want to drop? (1,2,3 for inventory or 0 for equipped item.)","Press 'esc' to cancel."), round % 3 == 0)
        case _ => ()
      }
    }
  }
}
