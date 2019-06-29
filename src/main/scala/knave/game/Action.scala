package knave.game

import knave.world.{BarrierCollision, EnemyCollision, NoCollision, World}
import knave.world.dungeon.{Coord, Direction}
import knave.world.dungeon.Dungeon._
import knave.world.item.WeaponItem

import scala.collection.mutable.ListBuffer
import scala.util.Random
import knave.display.Palette._
import knave.world.enemy.Alerted
import knave.world.player.weapon.{Fist, Weapon}

import scala.annotation.tailrec

sealed trait Action {
  def updateWorld(w : World) : Seq[Action]

  final protected def addLog(l : String) : Unit =
    Action.logs += l

  final protected def color(message : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + message + "</span>"
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    import w.dungeon
    w.checkCollision(c) match {
      case NoCollision =>
        w.player.pos = c
        w.player.refreshFieldOfVision
        if(w.itemAt(c).isDefined)
          addLog(s"A ${w.itemAt(c).get.name} lies at your feet. Press 'g' to pick it up.")
        if(w.dungeon.isStairs(c))
          addLog(s"You have reached the stairs. Press '<' to ascend.")
        Seq()

      case EnemyCollision(id) => w.player.weapon.attack(id) +: Seq()

      case BarrierCollision if w.dungeon.isDoor(c) =>
        w.dungeon.openDoor(c)
        w.player.refreshFieldOfVision
        Seq()

      case _ => Seq()
    }
  }
}

case class PickUpItem(c : Coord) extends  Action {
  override def updateWorld(w: World): Seq[Action] = {
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
        else addLog(s"Your inventory is full! Use 't' to drop one of your items on an Seq() tile.")

      case _ =>
    }
    Seq()
  }
}

case class EquipFromInventory(index : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
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
    Seq()
  }
}

case class DropWeapon(index : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] =
    w.player.inventory(index) match {
      case Some(weapon) =>
        w.player.inventory(index) = None
        addLog(s"You drop the ${weapon.name}.")
        SpawnWeapon(weapon,w.player.pos) +: Seq()

      case None => Seq()
    }
}

case object DropEquippedWeapon extends Action {
  override def updateWorld(w: World): Seq[Action] =
    if(w.player.weapon != Fist) {
      val weapon = w.player.weapon
      w.player.equipWeapon(Fist)
      addLog(s"You drop the ${weapon.name}.")
      Vector(SpawnWeapon(weapon,w.player.pos))
    }
    else Seq()
}

case class EnemyMove(id : Int, c : Coord, openDoor : Boolean) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    import w.dungeon

    w.checkCollision(c) match {
      case NoCollision =>
        w.enemy(id).foreach(e => {
          e.facing = Direction(e.pos, c)
          e.pos = c
          e.fieldOfVision = e.awareness match {
            case Alerted => e.pos.walkableDisk(e.vision).toSet
            case _ => e.pos.enemyConeOfVision(e.vision, e.facing).toSet
          }
        }); Seq()

      case BarrierCollision if w.dungeon.isDoor(c) && openDoor =>
        w.dungeon.openDoor(c)
        w.enemy(id).foreach(e => {
          e.fieldOfVision = e.pos.enemyConeOfVision(e.vision, e.facing).toSet
        }); Seq()

      case _ => Seq()
    }
  }
}

case class AttackOnEnemy(id : Int, damage : Int, cost: Int, melee: Boolean) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    w.enemy(id).toVector.flatMap(e => {
      e.hp -= damage
      addLog(s"You did ${damage} damage to the ${e.description}.")

      if(e.hp <= 0) Vector(DamagePlayerWeapon(cost), EnemyDeath(id))
      else Vector(DamagePlayerWeapon(cost))
    })
  }
}

case class EnemyDeath(id : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = w.enemy(id).toVector.flatMap(enemy => {
      w.destroyEnemy(id)
      w.dungeon.createCorpse(enemy.pos)
      Random.shuffle(enemy.pos.adjacent).take(enemy.blood).foreach(w.dungeon.bloodyTile)
      enemy.maybeDrop.filter(_._1 >= w.rng.nextDouble).map(it => SpawnWeapon(it._2,enemy.pos))
    })
}

case class AttackOnPlayer(enemyName : String, damage : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    w.player.hp -= damage
    addLog(s"${enemyName} did ${damage} damage to you.")
    if(w.player.hp <= 0) addLog(color("You have been slain!", red))
    Seq()
  }
}

case class DamagePlayerWeapon(damage : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    w.player.weapon.durability -= damage
    if(w.player.weapon.durability <= 0) {
      w.player.destroyWeapon
      addLog(color("Your weapon broke!", red))
    }
    Seq()
  }
}

case object AscendStairs extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    if(w.dungeon.isStairs(w.player.pos))
      w.player.ascended = true
    Seq()
  }
}

case class HealPlayer(heal : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    val trueHeal = if(w.player.hp + heal > w.player.maxHp) w.player.maxHp - w.player.hp else heal
    w.player.hp += trueHeal
    addLog(s"You have been healed for ${trueHeal} health.")
    Seq()
  }
}

case class HealEnemy(id : Int, heal : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = w.enemy(id).flatMap(enemy => {
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
  override def updateWorld(w: World): Seq[Action] = {
    w.addItem(WeaponItem(weapon,c))
    Seq()
  }
}

case class Log(message : String, color : String) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    addLog(color(message,color))
    Seq()
  }
}

object Action {

  private val logs = new ListBuffer[String]

  @tailrec
  def applyActions(w : World, actions : Seq[Action]) : Seq[String] = actions match {
    case a +: as => applyActions(w, a.updateWorld(w) ++ as)
    case Seq() => logs.toList  
  }

  implicit class ActionAlternative(as: Seq[Action]) {
    def tryOrElse(bs: Seq[Action]): Seq[Action] = {
      if(as.nonEmpty) as else bs
    }
  }
}

