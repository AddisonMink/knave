package knave.display

import knave.display.Palette._
import knave.game.{Fast, Slow}
import knave.world._
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Span}
import knave.world.enemy.{Alerted, Cautious, Enemy}
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.{Fist, Ray, Use}

trait Display {
  import Dungeon.{width,height}

  protected val prompt = document.getElementById("prompt").asInstanceOf[Div]

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
      }}
      row.appendChild(col)
    }
    map.appendChild(row)
  }

  protected final val hud = document.getElementById("hud").asInstanceOf[Div]

  protected final val inventory = document.getElementById("inventory").asInstanceOf[Div]

  protected final val tileWidth = 10

  protected final val tileHeight = 16

  protected final def clearMap : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width) {
        show(Coord(x, y), " ", white, Some(black))
        document.getElementById(x + "-" + y).asInstanceOf[Span].style.fontWeight = "normal"
      }

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

  protected final def createLog(logs : Seq[Log]) : String = {
    val logsToShow = logs.take(4).map(showLog).reverse
    (Seq.fill(4 - logsToShow.length)(" ") ++ logsToShow).mkString("\n")
  }

  protected final def showLog(log: Log): String = log match {
    case PlainLog(msg, clr) => color(msg,clr)
    case _ => ""
  }

  def display(w : World, speedRound : Boolean, promptStr: String = " ") : Unit = {
    clearMap
    prompt.innerHTML = promptStr
    map.style.display = "inline-block"
  }

  protected final def createHud(w : World) : String = {
    val str = new StringBuilder
    val p = w.player

    val hidden =
      if(w.getEnemies.exists(_.awareness == Alerted)) color("Alert",red)
      else if(w.getEnemies.exists(_.awareness == Cautious)) color("Caution",orange)
      else color("Hidden",green)

    str ++= s"Knave\t${hidden}\n"

    str ++= s"Round:\t${w.round}\n"

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
          case Use(_,_,description) => color(s"\n\tSpecial: ${description}", w.color)
          case knave.world.player.weapon.Circle(damage, cost) => color(s"\n\tSpecial: ${damage}, ${cost}, attack in a circle.", w.color)
          case _ => color("\n\tNo Special", w.color)
        }
        line1 + line2 + line3
      }
    }
    str ++= weapon

    str.toString
  }

  protected final def createInventory(p : Player) : String = {
      s"""Inventory
        |1. ${p.inventory(0).map(_.name).getOrElse("no item")}
        |2. ${p.inventory(1).map(_.name).getOrElse("no item")}
        |3. ${p.inventory(2).map(_.name).getOrElse("no item")}
      """.stripMargin
  }

  final def displayLook(w : World, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if((mouse != oldMouse || stateChanged) && w.player.fieldOfVision.contains(mouse)) {
      val maybeLog = w.checkCollision(mouse) match {
        case PlayerCollision => Some(PlainLog("You are here."))
        case EnemyCollision(id) => Some(PlainLog(w.enemy(id).map(_.description).getOrElse("") + " Press 'm' for more."))
        case _ if w.itemAt(mouse).nonEmpty => Some(PlainLog(s"A ${w.itemAt(mouse).get.name} lies on the ground." + " Press 'm' for more."))
        case _ => None
      }

      // This is the ONLY place where any public state changes are made out side of an Action (Actually not quite true. Enemy.spotPlayer makes public state changes. That shouldn't be!)
      w.logs = maybeLog.toSeq ++ w.logs

      display(w, speedRound, "You are in look mode. Press 'esc' to exit.")
    }

  final def displayRayAttack(w : World, range : Int, stateChanged : Boolean, speedRound : Boolean) : Unit =
    if(mouse != oldMouse || stateChanged) {
      import w.dungeon
      display(w, speedRound)
      val ray = w.player.pos.walkableLineTo(mouse).take(range)
      for(c <- ray)
        show(c, "*", white)
      prompt.innerHTML = "Select target with mouse. Press 'f' to confirm or 'esc' to cancel."
    }

  final def displayLogMore(w: World): Unit = {
    map.style.display = "none"
    log.innerHTML = (w.logs.map(showLog) :+ "You are in log mode. Pres 'esc' to exit.").mkString("\n")
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
