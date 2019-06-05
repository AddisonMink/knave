package knave.world.enemy

import knave.game.{Action, AttackOnPlayer, EnemyMove}
import knave.world.{NoCollision, World}
import knave.world.dungeon.Coord
import knave.world.dungeon.Dungeon.DungeonCoord

import Vector.empty

/**
  * Attack the player if he's in range. Otherwise do nothing.
  */
trait AttackingEnemy extends Enemy {

  protected val attackRange: Int

  protected val attackDamage: Int

  protected def attack(w: World): Vector[Action] =
    if(pos.distance(w.player.pos) <= attackRange)
      Vector(AttackOnPlayer(name,attackDamage))
    else empty
}

/**
  * Attack the player if he's in range. Otherwise try to move towards him.
  */
trait PursuingEnemy extends Enemy with AttackingEnemy {

  private def moveTowardPlayer(w: World): Vector[Action] = {
    import w.dungeon
    pos.walkableLineTo(w.player.pos) match {
      case c +: cs if cs.contains(w.player.pos) => Vector(EnemyMove(id,c,canOpenDoors))
      case _ => empty
    }
  }

  protected def pursue(w: World): Vector[Action] =
    attack(w) tryOrElse moveTowardPlayer(w)
}

/**
  * Choose a random coord in your starting room and move along a path to that coord.
  * When you reach it, pause for 1 turn, then choose another coord and repeat.
  */
trait WanderingEnemy extends Enemy {

  protected var dest : Coord

  protected def wander(w : World) : Vector[Action] = {
    import w.dungeon

    if(pos == dest) { dest = room.randomCoord; empty }
    else pos.nextOnPathTo(dest).filter(w.checkCollision(_) == NoCollision) match {
      case Some(c) => Vector(EnemyMove(id,c,canOpenDoors))
      case None => dest = room.randomCoord; empty
    }
  }
}

/**
  * Find a path to the player and follow that path.
  */
trait TrackingEnemy extends Enemy {

  protected def track(w: World): Vector[Action] = {
    import w.dungeon
    pos.nextOnPathTo(w.player.pos, canOpenDoors).map(EnemyMove(id,_,canOpenDoors)).toVector
  }
}