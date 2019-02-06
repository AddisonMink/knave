package knave.display

import knave.display.DisplayFull.{display, mouse, show}
import knave.display.Palette._
import knave.game.{Fast, Slow}
import knave.world.{EnemyCollision, NoCollision, PlayerCollision, World}
import knave.world.dungeon.{Coord, Dungeon}
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Span}
import knave.world.dungeon.Size._
import knave.world.enemy.Enemy
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.{Fist, Ray}

trait Display {

  protected val log = document.getElementById("log").asInstanceOf[Div]

  protected var oldMouse = Coord(0,0)

  protected var mouse = Coord(0,0)

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

  protected final val hud = document.getElementById("hud").asInstanceOf[Div]

  protected final val tileWidth = 10

  protected final val tileHeight = 16

  protected final def clearMap : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        show(Coord(x,y)," ", white, Some(black))

  protected final def show(c : Coord, symbol : String, color : String, backgroundColor : Option[String] = None) : Unit = {
    val tile = document.getElementById(c.x.toString + "-" + c.y.toString).asInstanceOf[Span]
    if(backgroundColor.isEmpty) {
      val bgc = tile.style.backgroundColor
      tile.style.color = color
      tile.style.backgroundColor = bgc
    }
    else {
      tile.style.color = color
      tile.style.backgroundColor = backgroundColor.get
    }
    tile.innerHTML = symbol
  }

  protected final def color(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  protected final def setTile(d : Dungeon, c : Coord, light : Boolean = true, color : Option[String] = None, backgroundColor : Option[String] = None) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined)
      if(light) show(c, floor.get.symbol, color.getOrElse(floor.get.color),backgroundColor)
      else show(c, floor.get.symbol, floor.get.darkColor)
    else if(wall.isDefined)
      if(light) show(c, wall.get.symbol, color.getOrElse(wall.get.color),backgroundColor.orElse(wall.map(_.color)))
      else show(c, wall.get.symbol, wall.get.darkColor, Some(wall.get.darkColor))
    else if(door.isDefined)
      if(light) show(c, door.get.symbol, color.getOrElse(door.get.color),backgroundColor)
      else show(c, door.get.symbol, door.get.darkColor)
  }

  protected final def setItem(i : Item) : Unit =
    i match {
      case WeaponItem(w, c) => show(c, WeaponItem(w, c).symbol, w.color)
      case _ => ()
    }

  protected final def setPlayer(p : Player) : Unit =
    show(p.pos, "@", white)

  protected final def setEnemy(e : Enemy, speedRound : Boolean) : Unit = {
    /*
    val color =
      if (speedRound && e.speed == Fast) red
      else if (speedRound && e.speed == Slow) cyan
      else e.color
    */
    val style = document.getElementById(e.pos.x.toString + "-" + e.pos.y.toString).asInstanceOf[Span].style
    if(speedRound && e.speed == Fast) {
      style.fontWeight = "bold"
      style.fontStyle = "normal"
    }
    else if(speedRound && e.speed == Slow) {
      style.fontStyle = "italic"
      style.fontWeight = "normal"
    }
    else {
      style.fontStyle = "normal"
      style.fontWeight = "normal"
    }

    show(e.pos, e.symbol.toString, e.color)
  }

  private var fullLog = List[String]()
  protected final def createLog(logs : List[String]) : String = {
    fullLog = logs
    val ls = logs.length match {
      case l if l > 4 => logs.drop(l - 3) :+ "Press 'm' for more."
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

  def display(w : World, logs : List[String] = List(), speedRound : Boolean) : Unit = {
    clearMap
    map.style.display = "inline-block"
  }

  protected final def createHud(p : Player) : String = {
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

  final def displayLook(w : World, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if((mouse != oldMouse || stateChanged) && w.player.fieldOfVision.contains(mouse)) {
      val log = w.checkCollision(mouse) match {
        case PlayerCollision => "You are here."
        case EnemyCollision(id) => w.enemy(id).map(_.description).getOrElse("") + " Press 'm' for more."
        case _ if w.itemAt(mouse).nonEmpty => s"A ${w.itemAt(mouse).get.name} lies on the ground." + " Press 'm' for more."
        case _ => ""
      }
      if(log.nonEmpty) display(w, List(log, "You are in look mode. Look at points of interest with your mouse. Press 'esc' to exit."), speedRound)
      else display(w, List("You are in look mode. Look at points of interest with your mouse. Press 'esc' to exit."), speedRound)
    }

  final def displayRayAttack(w : World, range : Int, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if(mouse != oldMouse || stateChanged) {
      display(w, List("Select target with mouse. Press 'f' to confirm or 'esc' to cancel."), speedRound)
      val ray = w.dungeon.visibleLine(w.player.pos, mouse).take(range)
      for(c <- ray)
        show(c, "*", white)
    }

  final def displayLogMore : Unit = {
    map.style.display = "none"

    val str = new StringBuilder
    for(l <- fullLog)
      str ++= (l + "\n")
    str ++= "You are in log mode. Press 'esc' to exit."
    for(_ <- 0 until 4 - fullLog.length)
      str += '\n'
    log.innerHTML = str.toString
  }

  final def displayLookMore(w : World, stateChanged : Boolean) : Unit =
    if(stateChanged && w.player.fieldOfVision.contains(mouse)) {
      val text = w.checkCollision(mouse) match {
        case PlayerCollision => "You are here."
        case EnemyCollision(id) => w.enemy(id).get.fullDescription
        case _ if w.itemAt(mouse).nonEmpty => w.itemAt(mouse).get.description
        case _ => "There is nothing here."
      }
      map.style.display = "none"
      log.innerHTML = text + "\nPress 'esc' to exit."
    }
}
