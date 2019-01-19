package knave.display

import knave.game.{Fast, Slow}
import knave.world.{EnemyCollision, PlayerCollision, World}
import org.scalajs.dom.document
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Size.{height, width}
import knave.world.enemy.Enemy
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.{Fist, Ray}
import org.scalajs.dom.html.{Div, Span}
import Palette._

object Display {

  private val log = document.getElementById("log").asInstanceOf[Div]

  private var oldMouse = Coord(0,0)

  private var mouse = Coord(0,0)

  def mousePos : Coord =
    mouse

  val map = document.getElementById("map").asInstanceOf[Div]
  for(y <- 0 until height) {
    val row = document.createElement("div").asInstanceOf[Div]
    for(x <- 0 until width) {
      val col = document.createElement("span").asInstanceOf[Span]
      col.id = x.toString + "-" + y.toString
      col.onmousemove = { e => {
          oldMouse = mouse
          mouse = Coord(x,y)
        }
      }
      row.appendChild(col)
    }
    map.appendChild(row)
  }

  private val hud = document.getElementById("hud").asInstanceOf[Div]

  private val tileWidth = 10

  private val tileHeight = 16

  private def clearMap : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        show(Coord(x,y)," ", white)

  private def show(c : Coord, symbol : String, color : String) : Unit = {
    val tile = document.getElementById(c.x.toString + "-" + c.y.toString).asInstanceOf[Span]
    tile.style = s"color : ${color}"
    tile.innerHTML = symbol
  }

  private def color(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  private def setTile(d : Dungeon, c : Coord, color : Option[String] = None) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) show(c, floor.get.symbol, color.getOrElse(floor.get.color))
    else if(wall.isDefined) show(c, wall.get.symbol, color.getOrElse(wall.get.color))
    else if(door.isDefined) show(c, door.get.symbol, color.getOrElse(door.get.color))
  }

  private def setTileDark(d : Dungeon, c : Coord) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) show(c, floor.get.symbol, floor.get.darkColor)
    else if(wall.isDefined) show(c, wall.get.symbol, wall.get.darkColor)
    else if(door.isDefined) show(c, door.get.symbol, door.get.darkColor)
  }

  private def setDungeon(d : Dungeon) : Unit = {
    for(y <- 0 until height)
      for(x <- 0 until width)
        setTile(d, Coord(x,y))
  }

  private def setDungeonFov(d : Dungeon, cs : Iterable[Coord]) : Unit = {
    for(c <- d.visited)
      setTileDark(d, c)
    for(c <- cs)
      setTile(d, c)
  }

  private def setItems(is : Iterable[Item]) : Unit =
    for(i <- is)
      i match {
        case WeaponItem(w, c) => show(c, WeaponItem(w,c).symbol, w.color)
        case _ => ()
      }

  private def setPlayer(p : Player) : Unit =
    show(p.pos, "@", white)

  private def setEnemyFov(e : Enemy, playerFov : Set[Coord], w : World) : Unit =
    if(w.player.hidden) {
      val enemyFov = w.dungeon.fieldOfVision(e.pos, e.vision)
      for(c <- enemyFov.intersect(playerFov))
        setTile(w.dungeon, c, Some(orange))
    }
    else {
      val enemyFov = w.dungeon.fieldOfVision(e.pos, e.vision*2)
      for(c <- enemyFov.intersect(playerFov))
        setTile(w.dungeon, c, Some(red))
    }

  private def setEnemy(e : Enemy, playerFov : Set[Coord], w : World, speedRound : Boolean) : Unit = {
    val color =
      if (speedRound && e.speed == Fast) red
      else if (speedRound && e.speed == Slow) cyan
      else e.color
    show(e.pos, e.symbol.toString, color)
  }

  private def createHud(p : Player) : String = {
    val str = new StringBuilder

    val hidden = if(p.hidden) color("Hidden", green) else color("Alert",red)

    str ++= s"Knave\t${hidden}\n"


    val healthColor = p.hp.toFloat / p.maxHp.toFloat match {
      case x if x >= 0.75 => green
      case x if x >= 0.25 => yellow
      case _ => red
    }
    str ++= s"Health: ${color(p.hp + "%", healthColor)}\n"

    val weapon = p.weapon match {
      case Fist => {
        val line1 = s"Weapon: ${Fist.name} (inf)"
        val line2 = s"\n\tAttack: ${Fist.attackDamage} damage"
        val line3 = "\n\tNo Special"
        line1 + line2 + line3
      }
      case w => {
        val line1 = "Weapon: " + color(s"${w.name} (${w.durability} / ${w.maxDurability})", w.color)
        val line2 = color(s"\n\tAttack: ${w.attackDamage} damage, ${w.attackCost} durability", w.color)
        val line3 = w.special match {
          case Ray(range, damage, cost) => color(s"\n\tSpecial: ${damage} damage, ${cost} durability, ${range}-tile ray.", w.color)
          case _ => color("\n\tNo Special", w.color)
        }
        line1 + line2 + line3
      }
    }
    str ++= weapon

    str.toString
  }

  private def createLog(logs : List[String]) : String = {
    val ls = logs.length match {
      case l if l > 4 => logs.drop(l - 4)
      case 4 => logs
      case 3 => " " :: logs
      case 2 => " " :: " " :: logs
      case 1 => " " :: " " :: " " :: logs
      case 0 => " " :: " " :: " " :: " " :: Nil
    }

    val str = new StringBuilder
    for(l <- ls)
      str ++= l + "\n"
    str.toString
  }

  /*
  def displayFull(w : World, logs : List[String] = List()) : Unit = {
    clearMap
    setDungeon(w.dungeon)
    setItems(w.getItems)
    setPlayer(w.player)
    for(e <- w.getEnemies) setEnemy(e)
    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }
  */


  def display(w : World, logs : List[String] = List(), speedRound : Boolean) : Unit = {
    clearMap

    val fov = w.dungeon.fieldOfVision(w.player.pos, w.player.vision)
    w.dungeon.visitCoords(fov)

    setDungeonFov(w.dungeon, fov)

    setItems(w.getItems)
    setPlayer(w.player)
    val enemies = w.getEnemies.filter(e => fov.contains(e.pos))
    for(e <- enemies) setEnemyFov(e, fov, w)
    for(e <- enemies) setEnemy(e, fov, w, speedRound)

    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }

  def displayLook(w : World, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if(mouse != oldMouse || stateChanged) {
      val log = w.checkCollision(mouse) match {
        case PlayerCollision => "You are here."
        case EnemyCollision(id) => w.enemy(id).map(_.description).getOrElse("")
        case _ if w.itemAt(mouse).nonEmpty => w.itemAt(mouse).get.name
        case _ => ""
      }
      if(log.nonEmpty) display(w, List(log, "You are in look mode. Look at points of interest with your mouse. Press 'esc' to exit."), speedRound)
      else display(w, List("You are in look mode. Look at points of interest with your mouse. Press 'esc' to exit."), speedRound)
    }


  def displayRayAttack(w : World, range : Int, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if(mouse != oldMouse || stateChanged) {
      display(w, List("Select target with mouse. Press 'f' to confirm or 'esc' to cancel."), speedRound)
      val ray = w.dungeon.visibleLine(w.player.pos, mouse).take(range)
      for(c <- ray)
        show(c, "*", red)
    }
}
