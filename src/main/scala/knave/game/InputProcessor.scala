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

  def process(w : World, input : String, mouse : Coord) : Seq[Action] =
    internalState match {
      case Start => processStart(w, input)
      case Look => processLook(input)
      case RayAttack(range, damage, cost) => processRayAttack(w, input, mouse, range, damage, cost)
      case LogMore => processLogMore(input)
      case LookMore => processLookMore(input)
      case Drop => processDrop(input)
    }

  def processStart(w : World, input : String) : Seq[Action] = {
    val pos = w.player.pos
    def move(c : Coord) = Seq(PlayerMove(c))
    input match {
      case "r" => Noop +: Seq()
      case "w" => move(pos.copy(y = pos.y - 1))
      case "s" => move(pos.copy(y = pos.y + 1))
      case "a" => move(pos.copy(x = pos.x - 1))
      case "d" => move(pos.copy(x = pos.x + 1))
      case "q" => move(Coord(pos.x - 1, pos.y - 1))
      case "e" => move(Coord(pos.x + 1, pos.y - 1))
      case "z" => move(Coord(pos.x - 1, pos.y + 1))
      case "c" => move(Coord(pos.x + 1, pos.y + 1))
      case "g" => if(w.itemAt(w.player.pos).nonEmpty) Seq(PickUpItem(w.player.pos)) else Seq()
      /*
      case "space" => {
        internalState = Look
        Seq()
      }
      */
      case "space" => Seq()
      case "f" => w.player.weapon.special match {
          case NoSpecial => Seq()
          case Ray(range, damage, cost) => {
            internalState = RayAttack(range, damage, cost)
            Seq()
          }
          case Use(effect, cost, _) => effect(w) :+ DamagePlayerWeapon(cost)
          case Circle(damage, cost) => w.player.pos.adjacent.map(w.checkCollision).collect{case EnemyCollision(id) => AttackOnEnemy(id,damage,cost,true)}
        }
      case "<" =>
        if(w.dungeon.isStairs(w.player.pos)) Seq(AscendStairs)
        else Seq()
      case "m" => {
        internalState = LogMore
        Seq()
      }
      case "1" => {
        if(w.player.inventory(0).nonEmpty)
          Seq(EquipFromInventory(0))
        else Seq()
      }
      case "2" => {
        if(w.player.inventory(1).nonEmpty)
          Seq(EquipFromInventory(1))
        else Seq()
      }
      case "3" => {
        if(w.player.inventory(2).nonEmpty)
          Seq(EquipFromInventory(2))
        else Seq()
      }
      case "t" => {
        internalState = Drop
        Seq()
      }
      case _ => Seq()
    }
  }

  def processLook(input : String) : Seq[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Seq()
      }
      case "m" => {
        internalState = LookMore
        Seq()
      }
      case _ => Seq()
    }

  def processRayAttack(w : World, input : String, mouse : Coord, range : Int, damage : Int, cost : Int) : Seq[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Seq()
      }
      case "f" => {
        import w.dungeon
        internalState = Start
        w.player.pos.walkableLineTo(mouse).take(range).map(w.checkCollision).collectFirst {
          case EnemyCollision(id) => AttackOnEnemy(id, damage, cost, false)
        }.toSeq
      }
      case _ => Seq()
    }

  def processLogMore(input : String) : Seq[Action] =
    if(input == "escape") {
      internalState = Start
      Seq()
    }
    else Seq()

  def processLookMore(input : String) : Seq[Action] =
    if(input == "escape") {
      internalState = Start
      Seq()
    }
    else Seq()

  def processDrop(input : String) : Seq[Action] =
    input match {
      case "escape" => {
        internalState = Start
        Seq()
      }
      case "0" => {
        internalState = Start
        Seq(DropEquippedWeapon)
      }
      case "1" => {
        internalState = Start
        Seq(DropWeapon(0))
      }
      case "2" => {
        internalState = Start
        Seq(DropWeapon(1))
      }
      case "3" => {
        internalState = Start
        Seq(DropWeapon(2))
      }
      case _ => Seq()
    }
}
