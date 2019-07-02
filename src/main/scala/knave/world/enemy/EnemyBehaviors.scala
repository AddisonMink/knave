package knave.world.enemy

import knave.game.{Action, AttackOnPlayer, EnemyMove}
import knave.world.{NoCollision, World}
import knave.world.dungeon.Coord
import knave.world.dungeon.Dungeon.DungeonCoord

/**
  * Attack the player if he's in range. Otherwise do nothing.
  */
protected trait AttackingEnemy extends Enemy {

  protected val attackRange: Int

  protected val attackDamage: Int

  protected def attack(w: World): Seq[Action] =
    if(pos.distance(w.player.pos) <= attackRange) AttackOnPlayer(name,attackDamage) +: Seq()
    else Seq()
}

/**
  * Attack the player if he's in range. Otherwise try to move towards him.
  */
protected trait PursuingEnemy extends Enemy with AttackingEnemy {

  private def moveTowardPlayer(w: World): Seq[Action] = {
    import w.dungeon
    println(pos.walkableLineTo(w.player.pos))
    pos.walkableLineTo(w.player.pos) match {
      case c +: cs if cs.contains(w.player.pos) => EnemyMove(id,c,canOpenDoors) +: Seq()
      case _ => Seq()
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
      case None => wanderDest = Some(room.randomCoord); Seq()
      case Some(dest) => goToDestination(w,dest) tryOrElse {wanderDest = Some(room.randomCoord); Seq()}
    }
  }
}