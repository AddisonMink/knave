package knave.world.enemy

import knave.game.{Action, AttackOnPlayer, EnemyMove}
import knave.world.{NoCollision, World}
import knave.world.dungeon.Coord
import knave.world.dungeon.Dungeon.DungeonCoord

import Vector.empty

/**
  * Attack the player if he's in range. Otherwise do nothing.
  */
protected trait AttackingEnemy extends Enemy {

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
protected trait PursuingEnemy extends Enemy with AttackingEnemy {

  private def moveTowardPlayer(w: World): Vector[Action] = {
    import w.dungeon
    pos.walkableLineTo(w.player.pos) match {
      case c +: cs if cs.contains(w.player.pos) => Vector(EnemyMove(id,c,canOpenDoors))
      case _ => empty
    }
  }

  protected def pursue(w: World): Seq[Action] = {
    if(fieldOfVision.contains(w.player.pos)) {
      lastKnownPlayerPos = Some(w.player.pos)
      attack(w) tryOrElse moveTowardPlayer(w)
    }
    else { awareness = Cautious; Seq() }
  }
}

/**
  * Choose a random coord in your starting room and move along a path to that coord.
  * When you reach it, pause for 1 turn, then choose another coord and repeat.
  */
protected trait WanderingEnemy extends Enemy {

  protected var wanderDest: Option[Coord] = None

  protected def wander(w : World) : Seq[Action] = {
    import w.dungeon.rng
    wanderDest match {
      case None => wanderDest = Some(room.randomCoord); empty
      case Some(dest) => goToDestination(w,dest) tryOrElse {wanderDest = Some(room.randomCoord); empty}
    }
  }
}