package knave.game

import knave.world.dungeon.Coord
import knave.world.dungeon.Dungeon._
import knave.world.player.weapon.{Circle, NoSpecial, Ray, Use}
import knave.world.{EnemyCollision, World}

object InputProcessor {

  sealed trait InputState
  case object Start extends InputState
  case object Look extends InputState
  case class RayAttack(range : Int, damage : Int, cost : Int) extends InputState
  case object LogMore extends InputState
  case object LookMore extends InputState
  case object Drop extends InputState

  private var internalState : InputState = Start
  def state = internalState

  def process(w : World, input : String, mouse : Coord) : Vector[Action] =
    internalState match {
      case Start => processStart(w, input)
      case Look => processLook(input)
      case RayAttack(range, damage, cost) => processRayAttack(w, input, mouse, range, damage, cost)
      case LogMore => processLogMore(input)
      case LookMore => processLookMore(input)
      case Drop => processDrop(input)
    }

  def processStart(w : World, input : String) : Vector[Action] = {
    val pos = w.player.pos
    def move(c : Coord) = Vector(PlayerMove(c))
    input match {
      case "w" => move(pos.copy(y = pos.y - 1))
      case "s" => move(pos.copy(y = pos.y + 1))
      case "a" => move(pos.copy(x = pos.x - 1))
      case "d" => move(pos.copy(x = pos.x + 1))
      case "q" => move(Coord(pos.x - 1, pos.y - 1))
      case "e" => move(Coord(pos.x + 1, pos.y - 1))
      case "z" => move(Coord(pos.x - 1, pos.y + 1))
      case "c" => move(Coord(pos.x + 1, pos.y + 1))
      case "g" => if(w.itemAt(w.player.pos).nonEmpty) Vector(PickUpItem(w.player.pos)) else Vector()
      case "space" => {
        internalState = Look
        Vector()
      }
      case "f" => w.player.weapon.special match {
          case NoSpecial => Vector()
          case Ray(range, damage, cost) => {
            internalState = RayAttack(range, damage, cost)
            Vector()
          }
          case Use(effect, cost, description) => effect(w) ++ Vector(DamagePlayerWeapon(cost))
          case Circle(damage, cost) => w.player.pos.adjacent.map(w.checkCollision(_)).collect{case EnemyCollision(id) => AttackOnEnemy(id,damage,true)}.toVector :+ DamagePlayerWeapon(cost)
        }
      case "<" =>
        if(w.dungeon.isStairs(w.player.pos)) Vector(AscendStairs)
        else Vector()
      case "m" => {
        internalState = LogMore
        Vector()
      }
      case "1" => {
        if(w.player.inventory(0).nonEmpty)
          Vector(EquipFromInventory(0))
        else Vector()
      }
      case "2" => {
        if(w.player.inventory(1).nonEmpty)
          Vector(EquipFromInventory(1))
        else Vector()
      }
      case "3" => {
        if(w.player.inventory(2).nonEmpty)
          Vector(EquipFromInventory(2))
        else Vector()
      }
      case "t" => {
        internalState = Drop
        Vector()
      }
      case _ => Vector()
    }
  }

  def processLook(input : String) : Vector[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Vector()
      }
      case "m" => {
        internalState = LookMore
        Vector()
      }
      case _ => Vector()
    }

  def processRayAttack(w : World, input : String, mouse : Coord, range : Int, damage : Int, cost : Int) : Vector[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Vector()
      }
      case "f" => {
        import w.dungeon
        internalState = Start
        val target = w.player.pos.walkableLineTo(mouse).take(range).map(c => w.checkCollision(c)).find(_.isInstanceOf[EnemyCollision]).map(_.asInstanceOf[EnemyCollision].id)
        target match {
          case None => Vector()
          case Some(id) => Vector(AttackOnEnemy(id, damage, false), DamagePlayerWeapon(cost))
        }
      }
      case _ => Vector()
    }

  def processLogMore(input : String) : Vector[Action] =
    if(input == "escape") {
      internalState = Start
      Vector()
    }
    else Vector()

  def processLookMore(input : String) : Vector[Action] =
    if(input == "escape") {
      internalState = Start
      Vector()
    }
    else Vector()

  def processDrop(input : String) : Vector[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Vector()
      }
      case "0" => {
        internalState = Start
        Vector(DropEquippedWeapon)
      }
      case "1" => {
        internalState = Start
        Vector(DropWeapon(0))
      }
      case "2" => {
        internalState = Start
        Vector(DropWeapon(1))
      }
      case "3" => {
        internalState = Start
        Vector(DropWeapon(2))
      }
      case _ => Vector()
    }
}
