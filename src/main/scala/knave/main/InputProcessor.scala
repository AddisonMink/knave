package knave.main

import knave.game.{Action, PlayerMove}
import knave.world.World
import knave.world.dungeon.Coord

object InputProcessor {

  sealed trait InputState
  case object Start extends InputState

  private var internalState = Start
  def state = internalState

  def process(w : World, input : String) : Vector[Action] =
    internalState match {
      case Start => processStart(w, input)
    }

  def processStart(w : World, input : String) : Vector[Action] = {
    val pos = w.player.pos
    def move(c : Coord) = Vector(PlayerMove(c))
    input match {
      case "w" => move(pos.copy(y = pos.y - 1))
      case "s" => move(pos.copy(y = pos.y + 1))
      case "a" => move(pos.copy(x = pos.x - 1))
      case "d" => move(pos.copy(x = pos.x + 1))
      case _ => Vector()
    }
  }
}
