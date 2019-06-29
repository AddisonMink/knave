package knave.game

import knave.world._
import knave.world.dungeon.{Coord, Direction}
import knave.world.dungeon.Dungeon._
import knave.world.item.WeaponItem
import scala.util.Random
import knave.display.Palette._
import knave.world.enemy.Alerted
import knave.world.player.weapon.{Fist, Weapon}

sealed trait Action {
  def updateWorld(w : World) : Seq[Action]
}

case class PlayerMove(c : Coord) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    import w.dungeon
    w.checkCollision(c) match {
      case NoCollision =>
        w.player.pos = c
        w.player.refreshFieldOfVision
        if(w.itemAt(c).isDefined)
          w.logs = PlainLog(s"A ${w.itemAt(c).get.name} lies at your feet. Press 'g' to pick it up.") +: w.logs
        if(w.dungeon.isStairs(c))
          w.logs = PlainLog(s"You have reached the stairs. Press '<' to ascend.") +: w.logs
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
        w.logs = PlainLog(s"You pick up the ${weapon.name}. Press 'f' to use its special attack.") +: w.logs

      case (Some(WeaponItem(weapon,_)), _) =>
        val i = w.player.inventory.indexWhere(_.isEmpty)
        if(i > -1) {
          w.player.inventory(i) = Some(weapon)
          w.removeItemAt(c)
          w.logs = PlainLog(s"You pick up the ${weapon.name} and put it in your bag.") +: w.logs
        }
        else w.logs = PlainLog(s"Your inventory is full! Use 't' to drop one of your items on an Seq() tile.") +: w.logs

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
        w.logs = PlainLog(s"You drop the ${weapon.name}.") +: w.logs
        SpawnWeapon(weapon,w.player.pos) +: Seq()

      case None => Seq()
    }
}

case object DropEquippedWeapon extends Action {
  override def updateWorld(w: World): Seq[Action] =
    if(w.player.weapon != Fist) {
      val weapon = w.player.weapon
      w.player.equipWeapon(Fist)
      w.logs = PlainLog(s"You drop the ${weapon.name}.") +: w.logs
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
      w.logs = PlainLog(s"You did ${damage} damage to the ${e.description}.") +: w.logs

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
    w.logs = PlainLog(s"${enemyName} did ${damage} damage to you.") +: w.logs
    if(w.player.hp <= 0) w.logs = PlainLog("You have been slain!", red) +: w.logs
    Seq()
  }
}

case class DamagePlayerWeapon(damage : Int) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    w.player.weapon.durability -= damage
    if(w.player.weapon.durability <= 0) {
      w.player.destroyWeapon
      w.logs = PlainLog("Your weapon broke!",red) +: w.logs
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
    w.logs = PlainLog(s"You have been healed for ${trueHeal} health.") +: w.logs
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
      w.logs = PlainLog(s"${enemy.name} has been healed for ${trueHeal} health.") +: w.logs
    None
  }).toVector

}

case class SpawnWeapon(weapon : Weapon, c : Coord) extends Action {
  override def updateWorld(w: World): Seq[Action] = {
    w.addItem(WeaponItem(weapon,c))
    Seq()
  }
}

object Action {
  implicit class ActionAlternative(as: Seq[Action]) {
    def tryOrElse(bs: Seq[Action]): Seq[Action] = {
      if(as.nonEmpty) as else bs
    }
  }
}

