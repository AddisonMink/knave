package knave.display

import knave.display.Palette._
import knave.game.{Fast, Slow}
import knave.world._
import knave.world.dungeon._
import knave.world.dungeon.Dungeon._
import org.scalajs.dom.document
import org.scalajs.dom.html.{Div, Span}
import knave.world.enemy.{Alerted, Cautious, Enemy}
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.{Fist, Ray, Use}

object Display {
  import Dungeon.{width,height}

  private val prompt = document.getElementById("prompt").asInstanceOf[Div]

  private val log = document.getElementById("log").asInstanceOf[Div]

  private var cursor = Coord(0,0)
  def cursorPos: Coord = cursor

  val map = document.getElementById("map").asInstanceOf[Div]
  for(y <- 0 until height) {
    val row = document.createElement("div").asInstanceOf[Div]
    for(x <- 0 until width) {
      val col = document.createElement("span").asInstanceOf[Span]
      col.id = x.toString + "-" + y.toString
      row.appendChild(col)
    }
    map.appendChild(row)
  }

  private val hud = document.getElementById("hud").asInstanceOf[Div]

  private val inventory = document.getElementById("inventory").asInstanceOf[Div]

  private def clearMap : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width) {
        document.getElementById(x + "-" + y).asInstanceOf[Span].style.fontWeight = "normal"
        show(Coord(x, y), " ", white, Some(black))
      }

  private def show(c : Coord, symbol : String, color : String, backgroundColor : Option[String] = None) : Unit = {
    val tile = document.getElementById(c.x.toString + "-" + c.y.toString).asInstanceOf[Span]
    backgroundColor match {
      case Some(bgc) =>
        tile.style.color = color
        tile.style.backgroundColor = bgc

      case None =>
        val bgc = tile.style.backgroundColor
        tile.style.color = color
        tile.style.backgroundColor = bgc
    }
    tile.innerHTML = symbol
  }

  private def color(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  private def setTile(d : Dungeon, c : Coord, light : Boolean = true, color : Option[String] = None, backgroundColor : Option[String] = None) : Unit = {
    d.floorAt(c) orElse d.wallAt(c) orElse d.doorAt(c) match {

      case Some(Wall(clr,darkColor,symbol)) =>
        val cl = if(light) clr else darkColor
        show(c,symbol,cl,backgroundColor.orElse(Some(cl)))

      case Some(t: Tile) => {
        val clr = if(light) t.color else t.darkColor
        show(c,t.symbol,clr,backgroundColor)
      }
      case _ =>
    }
  }

  private def setItem(i : Item) : Unit =
    i match {
      case wi @ WeaponItem(w, c) => show(c, wi.symbol, w.color)
      case _ =>
    }

  private def setPlayer(p : Player) : Unit =
    show(p.pos, "@", white)

  private def setEnemy(e : Enemy, speedRound : Boolean) : Unit = {
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

  private def createLog(logs : Seq[Log]) : String = {
    val logsToShow = logs.take(4).map(showLog).reverse
    (Seq.fill(4 - logsToShow.length)(" ") ++ logsToShow).mkString("\n")
  }

  private def showLog(log: Log): String = log match {
    case PlainLog(msg, clr) => color(msg, clr)
    case AttackOnEnemyLog(name, damage, percentHealth) =>
      val condition = percentHealth match {
        case x if x > 1.0 => color("fortified", cyan)
        case x if x == 1.0 => "unharmed"
        case x if x > 0.74 => color("scratched", lightGray)
        case x if x > 0.25 => color("wounded", yellow)
        case x if x > 0 => color("near death", red)
        case _ => color("dead", red)
      }
      s"You did ${damage} damage to the ${name} [${condition}]"


    case _ => ""
  }

  private def setDungeon(d : Dungeon, p : Player) : Unit = {
    p.visitedTiles.foreach(setTile(d, _, false))
    p.fieldOfVision.foreach(setTile(d, _, true))
  }

  private def setEnemyFov(e : Enemy, w : World) : Unit = {
    val color = e.awareness match {
      case Alerted => red
      case _ => orange
    }
    e.fieldOfVision.intersect(w.player.fieldOfVision).foreach(setTile(w.dungeon,_,true,None,Some(color)))
  }

  private def createHud(w : World) : String = {
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

  private def createInventory(p : Player) : String = {
    s"""Inventory
       |1. ${p.inventory(0).map(_.name).getOrElse("no item")}
       |2. ${p.inventory(1).map(_.name).getOrElse("no item")}
       |3. ${p.inventory(2).map(_.name).getOrElse("no item")}
      """.stripMargin
  }

  def display(w : World, speedRound : Boolean, promptStr: String = " ") : Unit = {
    clearMap
    prompt.innerHTML = "> " + promptStr + "\n"
    map.style.display = "inline-block"
    setDungeon(w.dungeon, w.player)
    val enemies = w.getEnemies.filter(e => w.player.fieldOfVision.contains(e.pos))
    enemies.foreach(setEnemyFov(_,w))
    w.getItems.filter(i => w.player.fieldOfVision.contains(i.pos)).foreach(setItem(_))
    setPlayer(w.player)
    enemies.foreach(setEnemy(_,speedRound))
    log.innerHTML = createLog(w.logs)
    hud.innerHTML = createHud(w)
    inventory.innerHTML = createInventory(w.player) ++ s"\n\nLevel: ${w.depth}"
  }

  def displayLook(w : World, stateChanged : Boolean, speedRound : Boolean, input: String) : Unit = {
    val defaultMessage = "You are in look mode. Press 'wasdqezc' to move the cursor. Press 'esc' to exit."
    val instructions = "Press 'space' for more. Press 'esc' to exit."
    if(stateChanged)
      cursor = w.player.pos
    val newCursor = cursor + (input match {
      case "w" => (0,-1)
      case "a" => (-1,0)
      case "s" => (0,1)
      case "d" => (1,0)
      case "q" => (-1,-1)
      case "e" => (1,-1)
      case "z" => (-1,1)
      case "c" => (1,1)
      case _ => (0,0)
    })
    if(newCursor.inBounds)
      cursor = newCursor

    val maybeMessage = w.checkCollision(cursor) match {
      case PlayerCollision => Some("You are here." + " " + instructions)
      case EnemyCollision(id) => w.enemy(id).map(e => e.description + " " + instructions)
      case NoCollision => w.itemAt(cursor).map(i => s"A ${i.name}." + " " + instructions)
      case _ => None
    }
    val message = maybeMessage.filter(_ => w.player.fieldOfVision.contains(cursor)).getOrElse(defaultMessage)

    display(w,speedRound,message)
    val tile = document.getElementById(cursor.x.toString + "-" + cursor.y.toString).asInstanceOf[Span]
    tile.style.backgroundColor = white
  }

  def displayRayAttack(w : World, range : Int, stateChanged : Boolean, speedRound : Boolean, input: String) : Unit = {
    import w.dungeon
    val instructions = "Select a target with 'wasdgezc' and press 'f' to confirm the attack."
    displayLook(w,stateChanged,speedRound,input)
    if(w.player.pos.hasWalkableLineTo(cursor) && w.player.pos.distance(cursor) <= range)
      w.player.pos.lineTo(cursor).foreach(show(_,"*","white",None))

    if(stateChanged) {
      val maybeNearestEnemy = w.getEnemies.filter(e => e.pos.hasWalkableLineTo(w.player.pos) && e.pos.distance(w.player.pos) <= range).sortBy(_.pos.distance(w.player.pos)).headOption
      maybeNearestEnemy.foreach(e => cursor = e.pos)
    }

    prompt.innerHTML = ">" + instructions
  }

  def displayLogMore(w: World): Unit = {
    val divider = Seq.fill(Dungeon.width)('=').mkString + "\n"
    map.style.display = "none"
    prompt.innerHTML = "You are in log mode. Pres 'esc' to exit."
    log.innerHTML = divider + w.logs.take(Dungeon.height).map(showLog).mkString("\n")
  }

  def displayLookMore(w : World, stateChanged : Boolean) : Unit = {
    val divider = Seq.fill(Dungeon.width)('=').mkString + "\n"
    val instructions = "Press escape to exit."
    val defaultText = s"There is nothing here\n${instructions}"
    val text = divider + (w.checkCollision(cursor) match {
      case EnemyCollision(id) => w.enemy(id).map(_.fullDescription + "\n" + instructions)
      case NoCollision => w.itemAt(cursor).map(_.description + "\n" + instructions)
      case _ => None
    }).getOrElse(defaultText)
    map.style.display = "none"
    log.innerHTML = text
  }

  private def setFullDungeon(d : Dungeon) : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        setTile(d, Coord(x,y),true)

  def displayFullDungeon(dungeon: Dungeon) : Unit = {
    setFullDungeon(dungeon)
  }
}
