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

  def run(input : String, mousePos : Coord): Unit = state match {
    case Ongoing =>
      val oldState = InputProcessor.state
      val playerActions = InputProcessor.process(world, input, mousePos)

      if(playerActions.nonEmpty) {

        world.run(playerActions)
        display(world, world.speedRound)

        if(world.player.hp <= 0)
          state = Dead
        else if(world.player.ascended) {
          display(world, false)
          state = Ascended
        }
      } else InputProcessor.state match {
        case Start => if (InputProcessor.state != oldState) display(world, world.speedRound)
        case Look => displayLook(world, InputProcessor.state == oldState, world.speedRound)
        case InputProcessor.RayAttack(range, _, _) => displayRayAttack(world, range, InputProcessor.state == oldState, world.speedRound)
        case LogMore => if(InputProcessor.state != oldState) displayLogMore(world)
        case LookMore => displayLookMore(world, InputProcessor.state == oldState)
        case Drop => if (InputProcessor.state != oldState) display(world, world.speedRound, "Which item do you want to drop? (1,2,3 for inventory or 0 for equipped item.). Press 'esc' to cancel.")
        case _ => ()
      }

    case _ =>
  }
}
