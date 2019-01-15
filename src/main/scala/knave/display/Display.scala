package knave.display

import knave.world.{PlayerCollision, World}
import org.scalajs.dom.document
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Size.{height, width}
import knave.world.enemy.Enemy
import knave.world.item.{Item, WeaponItem}
import knave.world.player.Player
import knave.world.player.weapon.Fist
import org.scalajs.dom.html.{Div, Span}

object Display {

  private val log = document.getElementById("log").asInstanceOf[Div]

  val map = document.getElementById("map").asInstanceOf[Div]
  for(y <- 0 until height) {
    val row = document.createElement("div").asInstanceOf[Div]
    for(x <- 0 until width) {
      val col = document.createElement("span").asInstanceOf[Span]
      col.id = x.toString + y.toString
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
        document.getElementById(x.toString + y.toString).innerHTML = " "

  private def show(c : Coord, symbol : String, color : String) : Unit = {
    val tile = document.getElementById(c.x.toString + c.y.toString).asInstanceOf[Span]
    tile.style = s"color : ${color}"
    tile.innerHTML = symbol
  }

  private def color(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  private def setTile(d : Dungeon, c : Coord) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) show(c, ".", floor.get.color)
    else if(wall.isDefined) show(c, "#", wall.get.color)
    else if(door.isDefined) if (door.get.open) show(c, "/", door.get.color) else show(c, "+", door.get.color)
  }

  private def setTileDark(d : Dungeon, c : Coord) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) show(c, ".", floor.get.darkColor)
    else if(wall.isDefined) show(c, "#", wall.get.darkColor)
    else if(door.isDefined) if (door.get.open) show(c, "/", door.get.darkColor) else show(c, "+", door.get.darkColor)
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
    show(p.pos, "@", "white")

  private def setEnemy(e : Enemy) : Unit =
    show(e.pos, e.symbol.toString, e.color)

  private def createHud(p : Player) : String = {
    val str = new StringBuilder
    str ++= "Knave\n"
    val healthColor = p.hp.toFloat / p.maxHp.toFloat match {
      case x if x >= 0.75 => "green"
      case x if x >= 0.25 => "yellow"
      case _ => "red"
    }
    str ++= s"Health: ${color(p.hp + "%", healthColor)}\n"

    val weapon = p.weapon match {
      case Fist => s"Weapon: ${Fist.name} (inf)"
      case w => "Weapon: " + color(s"${w.name} (${w.durability} / ${w.maxDurability})", w.color)
    }
    str ++= weapon

    str.toString
  }

  private var logs = List("", "", "", "Welcome to Knave! Use 'wasd' to move.")
  private def createLog(newLogs : List[String]) : String = {
    logs = logs ++ newLogs
    logs = logs.drop(logs.length - 4)
    val str = new StringBuilder
    for(l <- logs)
      str ++= l + "\n"
    str.toString
  }

  def displayFull(w : World, logs : List[String] = List()) : Unit = {
    clearMap
    setDungeon(w.dungeon)
    setItems(w.getItems)
    setPlayer(w.player)
    for(e <- w.getEnemies) setEnemy(e)
    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }

  def display(w : World, logs : List[String] = List()) : Unit = {
    clearMap

    val fov = w.dungeon.fieldOfVisioin(w.player.pos, w.player.vision).toList
    w.dungeon.visitCoords(fov)
    setDungeonFov(w.dungeon, fov)
    setItems(w.getItems)
    setPlayer(w.player)
    for(e <- w.getEnemies.filter(e => fov.contains(e.pos))) setEnemy(e)
    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }

  def displayLook(w : World, mouse : Coord, oldMouse : Coord) : Unit =
    if(mouse != oldMouse) {
      val log = w.checkCollision(mouse) match {
        case PlayerCollision => "You are here."
        case _ => ""
      }
      if(log.nonEmpty) display(w, List(log))
    }

  def normalize(x : Int, y : Int) : Coord = {
    val nx = x / tileWidth
    val ny = y / tileHeight - 3
    val trueY =
      if(ny < 0) 0
      else if (ny >= height) height - 1
      else ny
    Coord(nx, trueY)
  }
}
