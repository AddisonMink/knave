package knave.game

import knave.world.{EnemyCollision, NoCollision, World}
import knave.world.dungeon.Coord
import knave.world.item.WeaponItem

import scala.collection.mutable.ListBuffer
import scala.util.Random

import knave.display.Palette._

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
          w.player.fieldOfVision = w.dungeon.fieldOfVision(w.player.pos, w.player.vision)
          w.player.visitedTiles ++= w.player.fieldOfVision
          if(w.itemAt(c).isDefined)
            addLog(s"A ${w.itemAt(c).get.name} lies at your feet. Press 'g' to pick it up.")
          if(w.dungeon.isStairs(c))
            addLog(s"You have reached the stairs. Press '<' to ascend.")
          Vector()
        }
        case EnemyCollision(id) => Vector(w.player.weapon.attack(id), DamagePlayerWeapon(w.player.weapon.attackCost))
        case _ => Vector()
      }
    }
    else if(w.dungeon.isDoor(c)) Vector(OpenDoor(c))
    else Vector()
}

case class PickUpItem(c : Coord) extends  Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.itemAt(c) match {
      case Some(WeaponItem(weapon,_)) => {
        w.player.equipWeapon(weapon)
        w.removeItemAt(c)
        addLog(s"You pick up the ${weapon.name}. Press 'f' to use its special attack.")
        Vector()
      }
      case _ => Vector()
    }
  }
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
          w.enemy(id).map(e => {
            e.facing = Coord(c.x - e.pos.x, c.y - e.pos.y)
            e.pos = c
            e.fieldOfVision = w.dungeon.cone(e.pos,e.facing,e.vision)
          })
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
        if(enemy.hp <= damage)
          Vector(EnemyDeath(id))
        else {
          enemy.hp -= damage
          addLog(s"You did ${damage} damage to the ${enemy.description}")
          Vector()
        }
      }
      case _ => Vector()
    }
}

case class EnemyDeath(id : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    w.enemy(id) match {
      case None => Vector()
      case Some(enemy) => {
        w.destroyEnemy(id)
        addLog(color(s"You have slain the ${enemy.name}!", red))
        w.dungeon.createCorpse(enemy.pos)
        for(c <- Random.shuffle(enemy.pos.adjacent).take(enemy.blood))
          w.dungeon.bloodyTile(c)

        Vector()
      }
    }
}

case class AttackOnPlayer(enemyName : String, damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hp -= damage
    addLog(s"${enemyName} did ${damage} damage to you.")
    if(w.player.hp <= 0) addLog(color("You have been slain!", red))
    Vector()
  }
}

case class DamagePlayerWeapon(damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.weapon.durability -= damage
    if(w.player.weapon.durability <= 0) {
      w.player.destroyWeapon
      addLog(color("Your weapon broke!", red))
    }
    Vector()
  }
}

case object AscendStairs extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    if(w.dungeon.isStairs(w.player.pos)) {
      w.player.ascended = true
      addLog(color("You win!", green))
    }
    Vector()
  }
}

case object SpotSplayer extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hidden = false
    addLog(color("You have been spotted!", red))
    Vector()
  }
}

case object HidePlayer extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hidden = true
    addLog(color("You've managed to lose your pursuers.",green))
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