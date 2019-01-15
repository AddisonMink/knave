package knave.main

import knave.game.{Action, PickUpItem, PlayerMove}
import knave.world.World
import knave.world.dungeon.Coord

object InputProcessor {

  sealed trait InputState
  case object Start extends InputState
  case object Look extends InputState

  private var internalState : InputState = Start
  def state = internalState

  def process(w : World, input : String) : Vector[Action] =
    internalState match {
      case Start => processStart(w, input)
      case _ => processLook(input)
    }

  def processStart(w : World, input : String) : Vector[Action] = {
    val pos = w.player.pos
    def move(c : Coord) = Vector(PlayerMove(c))
    input match {
      case "w" => move(pos.copy(y = pos.y - 1))
      case "s" => move(pos.copy(y = pos.y + 1))
      case "a" => move(pos.copy(x = pos.x - 1))
      case "d" => move(pos.copy(x = pos.x + 1))
      case "g" => if(w.itemAt(w.player.pos).nonEmpty) Vector(PickUpItem(w.player.pos)) else Vector()
      case "space" => {
        internalState = Look
        Vector()
      }
      case _ => Vector()
    }
  }

  def processLook(input : String) : Vector[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Vector()
      }
      case _ => Vector()
    }
}
