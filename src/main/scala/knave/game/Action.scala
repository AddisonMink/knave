package knave.game

import knave.world.{EnemyCollision, NoCollision, World}
import knave.world.dungeon.Coord

import scala.collection.mutable.ListBuffer

sealed trait Action {
  def updateWorld(w : World) : Vector[Action]

  final protected def addLog(l : String) : Unit =
    Action.logs += l

  final protected def color(message : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + message + "</span>"
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    if(w.dungeon.isWalkable(c)) {
      w.checkCollision(c) match {
        case NoCollision => {
          w.player.pos = c
          Vector()
        }
        case EnemyCollision(id) => Vector(w.player.weapon.attack(id), DamagePlayerWeapon(w.player.weapon.attackCost))
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
          addLog(color(s"You have slain the ${enemy.name}!", "light-green"))
          Vector()
        }
        else {
          enemy.hp -= damage
          val status = enemy.hp.toFloat / enemy.maxHp.toFloat match {
            case x if x == 1.0 => color("unharmed", "light-green")
            case x if x > 0.75 => color("scratched", "light-green")
            case x if x > 0.25 => color("wounded", "yellow")
            case _ => color("near death", "red")
          }
          addLog(s"You did ${damage} damage to the ${enemy.name} (${status}).")
          Vector()
        }
      }
      case _ => Vector()
    }
}

case class AttackOnPlayer(enemyName : String, damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hp -= damage
    addLog(s"${enemyName} did ${damage} damage to you.")
    if(w.player.hp <= 0) addLog(color("You have been slain!", "red"))
    Vector()
  }
}

case class DamagePlayerWeapon(damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.weapon.durability -= damage
    if(w.player.weapon.durability <= 0) {
      w.player.destroyWeapon
      addLog(color("Your weapon broke!", "red"))
    }
    Vector()
  }
}

object Action {

  private val logs = new ListBuffer[String]

  def applyActions(w : World, actions : Vector[Action]) : List[String] = {
    logs.clear()

    def loop(as : Vector[Action]) : Unit =
      if(as.nonEmpty) {
        val newActions = as.head.updateWorld(w)
        loop(newActions ++ as.tail)
      }
    loop(actions)

    logs.toList
  }
}