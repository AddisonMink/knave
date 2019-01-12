package knave.game

import knave.world.World
import knave.world.dungeon.Coord

sealed trait Action {
  def updateWorld(w : World) : Vector[Action]
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    val valid = w.dungeon.isWalkable(c)
    if(valid) w.player.pos = c
    Vector()
  }
}

object Action {
  def applyActions(w : World, actions : Vector[Action]) : Unit =
    if(actions.nonEmpty) {
      val newActions = actions.head.updateWorld(w)
      applyActions(w, newActions ++ actions.tail)
    }
}
