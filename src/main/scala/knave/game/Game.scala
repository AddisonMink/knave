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
          val nonSlowEnemies = world.getEnemies.filterNot(_.speed == Slow)
          val fastEnemies = world.getEnemies.filter(_.speed == Fast)

          val logs1 = Action.applyActions(world, playerActions ++ nonSlowEnemies.flatMap(_.act(world)))
          Action.applyActions(world,world.getEnemies.flatMap(_.spotPlayer(world)))
          val logs2 = Action.applyActions(world, fastEnemies.flatMap(_.act(world)))
          Action.applyActions(world,world.getEnemies.flatMap(_.spotPlayer(world)))

          display(world, logs1 ++ logs2, (round + 1) % 3 == 0)
        }
        else {
          val actions = playerActions ++ world.getEnemies.flatMap(_.act(world))
          val logs = Action.applyActions(world,actions)
          Action.applyActions(world,world.getEnemies.flatMap(_.spotPlayer(world)))

          display(world, logs, (round + 1) % 3 == 0)
        }

        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended) {
          display(world, Seq("You win!"), false)
          state = Ascended
        }

        round += 1
      }
      else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) display(world, Seq(), round % 3 == 0)
        case Look => displayLook(world, InputProcessor.state == oldState, round % 3 == 0)
        case InputProcessor.RayAttack(range, _, _) => displayRayAttack(world, range, InputProcessor.state == oldState, round % 3 == 0)
        case LogMore => if(InputProcessor.state != oldState) displayLogMore
        case LookMore => displayLookMore(world, InputProcessor.state == oldState)
        case Drop => if (InputProcessor.state != oldState) display(world, Seq("Which item do you want to drop? (1,2,3 for inventory or 0 for equipped item.)","Press 'esc' to cancel."), round % 3 == 0)
        case _ => ()
      }
    }
  }
}
