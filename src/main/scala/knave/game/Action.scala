package knave.game

import knave.world._
import knave.world.dungeon.{Coord, Direction}
import knave.world.item.WeaponItem
import knave.world.dungeon.Dungeon._

import scala.util.Random
import knave.display.Palette._
import knave.game.SharedActions.{AlertEnemy, SpawnWeapon}
import knave.world.enemy.{Alerted, Cautious, Enemy, Unaware}
import knave.world.player.weapon.{Fist, Weapon}

sealed trait Action {
  def applyToWorld(w : World): Result

  final def |(alternative: Action): Action = {
    val first = this
    new Action {
      override def applyToWorld(w: World): Result = first.applyToWorld(w) match {
        case Failed => alternative.applyToWorld(w)
        case result => result
      }
    }
  }

  final def >>(next: Action): Action = {
    val current = this
    new Action {
      override def applyToWorld(w: World): Result = NewActions(current,next)
    }
  }

  final def toResult: Result = NewActions(this)
}

sealed trait Result
case object Failed extends Result
case object Completed extends Result
case class NewActions(actions: Action*) extends Result

sealed trait ActionOnEnemy extends Action {

  val id: Int

  protected def applyToWorld(enemy: Enemy, w: World): Result

  override final def applyToWorld(w: World): Result = w.enemy(id) match {
    case Some(enemy) => applyToWorld(enemy,w)
    case None => Failed
  }
}

object SharedActions {

  case object Wait extends Action {
    override def applyToWorld(w: World): Result = Completed
  }

  case object Fail extends Action {
    override def applyToWorld(w: World): Result = Failed
  }

  case class SpawnWeapon(weapon: Weapon, c: Coord) extends Action {
    override def applyToWorld(w: World): Result = {
      w.addItem(WeaponItem(weapon,c))
      Completed
    }
  }

  case class AlertEnemy(id: Int) extends ActionOnEnemy {
    def applyToWorld(enemy: Enemy, w: World): Result = {
      import w.dungeon
      if(enemy.awareness != Alerted) {
        enemy.awareness = Alerted
        w.logs = PlainLog("You have been spotted!", red) +: w.logs
        enemy.refreshFieldOfVision
      }
      enemy.lastKnownPlayerPos = Some(w.player.pos)
      Completed
    }
  }
}

object PlayerActions {

  case class PlayerMove(c : Coord) extends Action {
    override def applyToWorld(w: World): Result = {
      import w.dungeon
      w.checkCollision(c) match {
        case NoCollision =>
          w.player.pos = c
          w.player.refreshFieldOfVision
          if(w.itemAt(c).isDefined)
            w.logs = PlainLog(s"A ${w.itemAt(c).get.name} lies at your feet. Press 'g' to pick it up.") +: w.logs
          if(w.dungeon.isStairs(c))
            w.logs = PlainLog(s"You have reached the stairs. Press '<' to ascend.") +: w.logs
          Completed

        case EnemyCollision(id) => NewActions(w.player.weapon.attack(id))

        case BarrierCollision if w.dungeon.isDoor(c) =>
          w.dungeon.openDoor(c)
          w.player.refreshFieldOfVision
          Completed

        case _ => Failed
      }
    }
  }

  case class PickUpItem(c : Coord) extends Action {
    override def applyToWorld(w: World): Result = {
      (w.itemAt(c), w.player.weapon) match {
        case (Some(WeaponItem(weapon,_)), Fist) =>
          w.player.equipWeapon(weapon)
          w.removeItemAt(c)
          w.logs = PlainLog(s"You pick up the ${weapon.name}. Press 'f' to use its special attack.") +: w.logs
          Completed

        case (Some(WeaponItem(weapon,_)), _) =>
          val i = w.player.inventory.indexWhere(_.isEmpty)
          if(i > -1) {
            w.player.inventory(i) = Some(weapon)
            w.removeItemAt(c)
            w.logs = PlainLog(s"You pick up the ${weapon.name} and put it in your bag.") +: w.logs
          }
          else w.logs = PlainLog(s"Your inventory is full! Use 't' to drop one of your items.") +: w.logs
          Completed

        case _ => Failed
      }
    }
  }

  case class EquipFromInventory(index : Int) extends Action {
    override def applyToWorld(w: World): Result = {
      (w.player.inventory(index), w.player.weapon) match {
        case (Some(weapon), Fist) =>
          w.player.equipWeapon(weapon)
          w.player.inventory(index) = None
          Completed

        case (Some(weapon), _) =>
          val oldWeapon = w.player.weapon
          w.player.equipWeapon(weapon)
          w.player.inventory(index) = Some(oldWeapon)
          Completed

        case _ => Failed
      }
    }
  }

  case class DropWeapon(index : Int) extends Action {
    override def applyToWorld(w: World): Result =
      w.player.inventory(index) match {
        case Some(weapon) =>
          w.player.inventory(index) = None
          w.logs = PlainLog(s"You drop the ${weapon.name}.") +: w.logs
          SpawnWeapon(weapon,w.player.pos).toResult

        case None => Failed
      }
  }

  case object DropEquippedWeapon extends Action {
    override def applyToWorld(w: World): Result =
      if(w.player.weapon != Fist) {
        val weapon = w.player.weapon
        w.player.equipWeapon(Fist)
        w.logs = PlainLog(s"You drop the ${weapon.name}.") +: w.logs
        SpawnWeapon(weapon,w.player.pos).toResult
      }
      else Failed
  }

  case class AttackOnEnemy(id : Int, damage : Int, cost: Int, melee: Boolean) extends ActionOnEnemy {
    override def applyToWorld(enemy: Enemy, w: World): Result =
      (DamageEnemy(enemy.id, damage) >> AlertEnemy(id) >> DamagePlayerWeapon(cost)).toResult
  }

  private case class DamageEnemy(id: Int, damage: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      println(enemy.hp)
      enemy.hp -= damage
      println(enemy.hp)
      w.logs = AttackOnEnemyLog(enemy.name, damage, enemy.hp.toDouble / enemy.maxHp.toDouble) +: w.logs
      if(enemy.hp <= 0) NewActions(EnemyDeath(enemy.id))
      else Completed
    }
  }

  private case class EnemyDeath(id: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      w.destroyEnemy(enemy.id)
      w.dungeon.createCorpse(enemy.pos)
      Random.shuffle(enemy.pos.adjacent).take(enemy.blood).foreach(w.dungeon.bloodyTile)
      w.logs = PlainLog(s"You have slain the ${enemy.name}.") +: w.logs
      enemy.maybeDrop match {
        case None => Completed
        case Some((chance,weapon)) =>
          val drop = w.rng.nextDouble <= chance
          if(drop) SpawnWeapon(weapon,enemy.pos).toResult
          else Completed
      }
    }
  }

  case class DamagePlayerWeapon(damage : Int) extends Action {
    override def applyToWorld(w: World): Result = {
      w.player.weapon.durability -= damage
      if(w.player.weapon.durability <= 0) {
        w.player.destroyWeapon
        w.logs = PlainLog("Your weapon broke!",red) +: w.logs
      }
      Completed
    }
  }

  case object AscendStairs extends Action {
    override def applyToWorld(w: World): Result = {
      if(w.dungeon.isStairs(w.player.pos)) {
        w.player.ascended = true
        Completed
      }
      Failed
    }
  }

  case class HealPlayer(heal : Int) extends Action {
    override def applyToWorld(w: World): Result = {
      val trueHeal = if(w.player.hp + heal > w.player.maxHp) w.player.maxHp - w.player.hp else heal
      w.player.hp += trueHeal
      w.logs = PlainLog(s"You have been healed for ${trueHeal} health.") +: w.logs
      Completed
    }
  }
}

object EnemyActions {

  case class SpotPlayer(id: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      (enemy.awareness, enemy.fieldOfVision.contains(w.player.pos)) match {
        case (_, true) => AlertEnemy(id).toResult
        case (Alerted, false) => enemy.awareness = Cautious; Completed
        case _ => Failed
      }
    }
  }

  case class EnemyMove(id : Int, c : Coord, openDoor : Boolean) extends ActionOnEnemy {
    def applyToWorld(enemy: Enemy, w: World): Result = {
      import w.dungeon
      w.checkCollision(c) match {
        case NoCollision =>
          enemy.facing = Direction(enemy.pos,c)
          enemy.pos = c
          enemy.refreshFieldOfVision
          Completed

        case BarrierCollision if w.dungeon.isDoor(c) && openDoor =>
          w.dungeon.openDoor(c)
          enemy.refreshFieldOfVision
          Completed

        case _ => Failed
      }
    }
  }

  case class AttackPlayer(enemyName : String, damage : Int) extends Action {
    override def applyToWorld(w: World): Result = {
      w.player.hp -= damage
      w.logs = PlainLog(s"${enemyName} did ${damage} damage to you.") +: w.logs
      if(w.player.hp <= 0) w.logs = PlainLog("You have been slain!", red) +: w.logs
      Completed
    }
  }

  case class ChasePlayer(id: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      val maybeMove = for {
        dest <- Some(w.player.pos)
        next <- enemy.pos.lineTo(dest).headOption if enemy.fieldOfVision.contains(dest) && enemy.awareness == Alerted
      } yield EnemyMove(id,next,enemy.canOpenDoors).toResult
      maybeMove match {
        case Some(move) => move
        case None if enemy.awareness == Alerted => enemy.awareness = Cautious; Completed
        case None => Failed
      }
    }
  }

  case class Patrol(id: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      import w.dungeon
      import w.dungeon.rng

      val maybeMove = for {
        dest <- enemy.patrolDestination if dest != enemy.pos
        next <- enemy.pos.findPath(dest,enemy.canOpenDoors).headOption
      } yield EnemyMove(id,next,enemy.canOpenDoors).toResult

      maybeMove.getOrElse{
        enemy.patrolDestination = Some(enemy.room.randomCoord)
        Completed
      }
    }
  }

  case class Investigate(id: Int) extends ActionOnEnemy {
    override protected def applyToWorld(enemy: Enemy, w: World): Result = {
      import w.dungeon

      val maybeMove = for {
        dest <- enemy.lastKnownPlayerPos if dest != enemy.pos
        next <- enemy.pos.findPath(dest,enemy.canOpenDoors).headOption
      } yield EnemyMove(id,next,enemy.canOpenDoors).toResult

      maybeMove.getOrElse{
        if(enemy.awareness == Cautious) {
          enemy.awareness = Unaware
          enemy.lastKnownPlayerPos = None
          Completed
        } else Failed
      }
    }
  }
}

