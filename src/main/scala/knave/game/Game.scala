package knave.game

import InputProcessor._
import knave.main._
import knave.world.World
import knave.display.Display._

import scala.util.Random

class Game(seed : Int = Random.nextInt) {

  println(seed)
  var world = World(seed)

  private var state : GameState = Ongoing

  def run(input : String): Unit = state match {
    case Ongoing =>
      val oldState = InputProcessor.state

      val playerActions = InputProcessor.process(world, input, cursorPos)
      if(playerActions.nonEmpty) {
        world.run(playerActions)
        if(world.player.hp <= 0) state = Dead
        else if(world.player.ascended) state = Ascended
      }

      InputProcessor.state match {
        case Start => display(world, world.speedRound)
        case Look => displayLook(world, InputProcessor.state != oldState, world.speedRound, input)
        case RayAttack(range, _, _) => displayRayAttack(world, range, InputProcessor.state != oldState, world.speedRound, input)
        case LogMore => if(InputProcessor.state != oldState) displayLogMore(world)
        case LookMore => displayLookMore(world, InputProcessor.state == oldState)
        case Drop => if (InputProcessor.state != oldState) display(world, world.speedRound, "Drop which item? (1,2,3 for inventory or 0 for equipped item.). Press 'esc' to cancel.")
        case _ => ()
      }

    case _ =>
  }
}
