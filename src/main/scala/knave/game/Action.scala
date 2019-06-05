package knave.game

import knave.world.{EnemyCollision, NoCollision, World}
import knave.world.dungeon.{Coord, Direction}
import knave.world.dungeon.Dungeon._
import knave.world.item.WeaponItem

import scala.collection.mutable.ListBuffer
import scala.util.Random
import knave.display.Palette._
import knave.world.player.weapon.{Fist, Weapon}

import Vector.empty
import scala.annotation.tailrec

sealed trait Action {
  def updateWorld(w : World) : Vector[Action]

  final protected def addLog(l : String) : Unit =
    Action.logs += l

  final protected def color(message : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + message + "</span>"
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    import w.dungeon

    (c.isWalkable, w.dungeon.isDoor(c), w.checkCollision(c)) match {
      case (true,_,NoCollision) =>
        w.player.pos = c
        w.player.fieldOfVision = c.visibleDisk(w.player.vision).toSet
        w.player.visitedTiles ++= w.player.fieldOfVision
        if(w.itemAt(c).isDefined)
          addLog(s"A ${w.itemAt(c).get.name} lies at your feet. Press 'g' to pick it up.")
        if(w.dungeon.isStairs(c))
          addLog(s"You have reached the stairs. Press '<' to ascend.")
        empty

      case (false,true,_) =>
        w.dungeon.openDoor(c)
        w.player.fieldOfVision = c.visibleDisk(w.player.vision).toSet
        w.player.visitedTiles ++= w.player.fieldOfVision
        empty

      case (_,_,EnemyCollision(id)) => Vector(w.player.weapon.attack(id), DamagePlayerWeapon(w.player.weapon.attackCost))

      case _ => empty
    }
  }
}

case class PickUpItem(c : Coord) extends  Action {
  override def updateWorld(w: World): Vector[Action] = {
    (w.itemAt(c), w.player.weapon) match {
      case (Some(WeaponItem(weapon,_)), Fist) =>
        w.player.equipWeapon(weapon)
        w.removeItemAt(c)
        addLog(s"You pick up the ${weapon.name}. Press 'f' to use its special attack.")

      case (Some(WeaponItem(weapon,_)), _) =>
        val i = w.player.inventory.indexWhere(_ == None)
        if(i > -1) {
          w.player.inventory(i) = Some(weapon)
          w.removeItemAt(c)
          addLog(s"You pick up the ${weapon.name} and put it in your bag.")
        }
        else addLog(s"Your inventory is full! Use 't' to drop one of your items on an empty tile.")

      case _ =>
    }
    empty
  }
}

case class EquipFromInventory(index : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    (w.player.inventory(index), w.player.weapon) match {
      case (Some(weapon), Fist) =>
        w.player.equipWeapon(weapon)
        w.player.inventory(index) = None

      case (Some(weapon), _) =>
        val oldWeapon = w.player.weapon
        w.player.equipWeapon(weapon)
        w.player.inventory(index) = Some(oldWeapon)

      case _ =>
    }
    empty
  }
}

case class DropWeapon(index : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] =
    w.player.inventory(index) match {
      case Some(weapon) =>
        w.player.inventory(index) = None
        addLog(s"You drop the ${weapon.name}.")
        Vector(SpawnWeapon(weapon,w.player.pos))

      case None => empty
    }
}

case object DropEquippedWeapon extends Action {
  override def updateWorld(w: World): Vector[Action] =
    if(w.player.weapon != Fist) {
      val weapon = w.player.weapon
      w.player.equipWeapon(Fist)
      addLog(s"You drop the ${weapon.name}.")
      Vector(SpawnWeapon(weapon,w.player.pos))
    }
    else empty
}

case class EnemyMove(id : Int, c : Coord, openDoor : Boolean) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    import w.dungeon

    (c.isWalkable, w.dungeon.isDoor(c), w.checkCollision(c)) match {
      case (true, _, NoCollision) =>
        for (e <- w.enemy(id)) {
          e.facing = Direction(e.pos, c)
          e.pos = c
          e.fieldOfVision = e.pos.enemyConeOfVision(e.vision, e.facing).toSet
        }
        empty

      case (false, true, _) if openDoor =>
        w.dungeon.openDoor(c)
        for(e <- w.enemy(id)) {
          e.fieldOfVision = e.pos.enemyConeOfVision(e.vision, e.facing).toSet
        }
        empty

      case _ => empty
    }
  }
}

case class AttackOnEnemy(id : Int, damage : Int, melee: Boolean) extends Action {
  override def updateWorld(w: World): Vector[Action] = w.enemy(id).flatMap(enemy => {
    if(w.player.hidden && melee) {
      enemy.hp -= damage * 2
      addLog(color("[backstab]",yellow) + s" You did ${damage*2} damage to the ${enemy.description}.")
    }
    else {
      enemy.hp -= damage
      addLog(s"You did ${damage} damage to the ${enemy.description}.")
    }

    if(enemy.hp <= 0) Some(EnemyDeath(id))
    else if(w.player.hidden) Some(SpotSplayer)
    else None
  }).toVector
}

case class EnemyDeath(id : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = w.enemy(id).flatMap(enemy => {
      w.destroyEnemy(id)
      w.dungeon.createCorpse(enemy.pos)
      for(c <- Random.shuffle(enemy.pos.adjacent).take(enemy.blood))
        w.dungeon.bloodyTile(c)
      if(Random.nextDouble <= enemy.dropRate)
        Some(SpawnWeapon(enemy.drop, enemy.pos))
      else
        None
    }).toVector
}

case class AttackOnPlayer(enemyName : String, damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hp -= damage
    addLog(s"${enemyName} did ${damage} damage to you.")
    if(w.player.hp <= 0) addLog(color("You have been slain!", red))
    empty
  }
}

case class DamagePlayerWeapon(damage : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.weapon.durability -= damage
    if(w.player.weapon.durability <= 0) {
      w.player.destroyWeapon
      addLog(color("Your weapon broke!", red))
    }
    empty
  }
}

case object AscendStairs extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    if(w.dungeon.isStairs(w.player.pos))
      w.player.ascended = true
    empty
  }
}

case object SpotSplayer extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hidden = false
    addLog(color("You have been spotted!", red))
    empty
  }
}

case object HidePlayer extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.player.hidden = true
    addLog(color("You've managed to lose your pursuers.",green))
    empty
  }
}

case class HealPlayer(heal : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    val trueHeal = if(w.player.hp + heal > w.player.maxHp) w.player.maxHp - w.player.hp else heal
    w.player.hp += trueHeal
    addLog(s"You have been healed for ${trueHeal} health.")
    empty
  }
}

case class HealEnemy(id : Int, heal : Int) extends Action {
  override def updateWorld(w: World): Vector[Action] = w.enemy(id).flatMap(enemy => {
    val trueHeal =
      if(enemy.hp + heal > enemy.fortifiedHp) enemy.fortifiedHp - enemy.hp
      else heal
    enemy.hp += trueHeal
    if(trueHeal > 0 &&  w.player.fieldOfVision.contains(enemy.pos))
      addLog(s"${enemy.name} has been healed for ${trueHeal} health.")
    None
  }).toVector

}

case class SpawnWeapon(weapon : Weapon, c : Coord) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    w.addItem(WeaponItem(weapon,c))
    empty
  }
}

case class Log(message : String, color : String) extends Action {
  override def updateWorld(w: World): Vector[Action] = {
    addLog(color(message,color))
    empty
  }
}

object Action {

  private val logs = new ListBuffer[String]

  def applyActions(w : World, actions : Vector[Action]) : List[String] = {
    logs.clear()

    @tailrec
    def loop(as : Vector[Action]) : Unit =
      if(as.nonEmpty) {
        val newActions = as.head.updateWorld(w)
        loop(newActions ++ as.tail)
      }
    loop(actions)

    logs.toList
  }

  implicit class ActionAlternative(as: Vector[Action]) {
    def tryOrElse(bs: Vector[Action]): Vector[Action] = {
      if(as.nonEmpty) as else bs
    }
  }
}

