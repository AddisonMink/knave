package knave.main

import knave.game._
import knave.world.{EnemyCollision, World}
import knave.world.dungeon.Coord
import knave.world.enemy.Enemy
import knave.world.player.weapon.{NoSpecial, Ray}

object InputProcessor {

  sealed trait InputState
  case object Start extends InputState
  case object Look extends InputState
  case class RayAttack(range : Int, damage : Int, cost : Int) extends InputState

  private var internalState : InputState = Start
  def state = internalState

  def process(w : World, input : String, mouse : Coord) : Vector[Action] =
    internalState match {
      case Start => processStart(w, input)
      case Look => processLook(input)
      case RayAttack(range, damage, cost) => processRayAttack(w, input, mouse, range, damage, cost)
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
          case _ => Vector()
        }
      case "<" =>
        if(w.dungeon.isStairs(w.player.pos)) Vector(AscendStairs)
        else Vector()
      case _ => Vector()
    }
  }

  def processLook(input : String) : Vector[Action] =
    input match {
      case "escape" => {
        internalState = Start
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
        internalState = Start
        val target = w.dungeon.visibleLine(w.player.pos, mouse).take(range).map(w.checkCollision).find(_.isInstanceOf[EnemyCollision]).map(_.asInstanceOf[EnemyCollision].id)
        target match {
          case None => Vector()
          case Some(id) => Vector(AttackOnEnemy(id, damage), DamagePlayerWeapon(cost))
        }
      }
      case _ => Vector()
    }
}
