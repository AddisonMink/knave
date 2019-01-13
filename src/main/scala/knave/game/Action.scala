package knave.game

import knave.world.{EnemyCollision, NoCollision, World}
import knave.world.dungeon.Coord

sealed trait Action {
  def updateWorld(w : World) : Vector[Action]
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    if(w.dungeon.isWalkable(c)) {
      w.checkCollision(c) match {
        case NoCollision => {
          w.player.pos = c
          Vector()
        }
        case EnemyCollision(id) => Vector(w.player.weapon.attack(id))
        case _ => Vector()
      }
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

case class EnemyMove(id : Int, c : Coord, openDoor : Boolean) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    if(w.dungeon.isWalkable(c)) {
      w.checkCollision(c) match {
        case NoCollision => {
          w.enemy(id).map(_.pos = c)
          Vector()
        }
        case _ => Vector()
      }
    }
    else if(openDoor && w.dungeon.isDoor(c)) {
      w.dungeon.openDoor(c)
      Vector()
    }
    else Vector()
  }
}

case class AttackOnEnemy(id : Int, damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    w.enemy(id) match {
      case None => Vector()
      case Some(enemy) => {
        if(enemy.hp <= damage) {
          w.destroyEnemy(id)
          println(s"You have slain the  ${enemy.name}!")
          Vector()
        }
        else {
          enemy.hp -= damage
          println(s"You did ${damage} damage to the ${enemy.name}.")
          Vector()
        }
      }
      case _ => Vector()
    }
}

object Action {
  def applyActions(w : World, actions : Vector[Action]) : Unit =
    if(actions.nonEmpty) {
      val newActions = actions.head.updateWorld(w)
      applyActions(w, newActions ++ actions.tail)
    }
}
