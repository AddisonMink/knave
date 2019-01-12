package knave.game

import knave.world.World
import knave.world.dungeon.Coord

sealed trait Action {
  def updateWorld(w : World) : Vector[Action]
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    if(w.dungeon.isWalkable(c)) {
      w.player.pos = c
      Vector()
    }
    else if(w.dungeon.isDoor(c)) Vector(OpenDoor(c))
    else Vector()
}

case class OpenDoor(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.dungeon.openDoor(c)
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
