package knave.world.enemy

import knave.game.{Action, EnemyMove}
import knave.world.{NoCollision, World}
import knave.world.dungeon.Coord

trait WanderingEnemy extends Enemy {

  protected var dest : Coord

  protected def wander(w : World) : Vector[Action] = {
    if(pos == dest) {
      dest = room.randomCoord(rng)
      Vector()
    }
    else
      w.dungeon.findPath(pos,dest).headOption.filter(w.checkCollision(_) == NoCollision) match {
        case Some(c) => Vector(EnemyMove(id,c,canOpenDoors))
        case None =>
          dest = room.randomCoord(rng)
          Vector()
      }
  }
}
