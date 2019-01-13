package knave.display

import knave.world.World
import org.scalajs.dom.document
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Size.{height, width}
import knave.world.enemy.Enemy
import knave.world.player.Player

object Display {

  private val map = document.getElementById("map")

  private val tileArray = Array.ofDim[String](height,width)

  private def resetTileArray : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        tileArray(y)(x) = " "
  resetTileArray

  private def buildString : String = {
    val str = new StringBuilder
    for(y <- 0 until height) {
      for (x <- 0 until width)
        str ++= tileArray(y)(x)
      str += '\n'
    }
    str.toString
  }

  private def show(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  private def setTile(d : Dungeon, c : Coord) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) tileArray(c.y)(c.x) = show(".", floor.get.color)
    else if(wall.isDefined) tileArray(c.y)(c.x) = show("#", wall.get.color)
    else if(door.isDefined) tileArray(c.y)(c.x) = if (door.get.open) show("/", door.get.color) else show("+", door.get.color)
  }

  private def setTileDark(d : Dungeon, c : Coord) : Unit = {
    lazy val floor = d.floorAt(c)
    lazy val wall = d.wallAt(c)
    lazy val door = d.doorAt(c)
    if(floor.isDefined) tileArray(c.y)(c.x) = show(".", floor.get.darkColor)
    else if(wall.isDefined) tileArray(c.y)(c.x) = show("#", wall.get.darkColor)
    else if(door.isDefined) tileArray(c.y)(c.x) = if (door.get.open) show("/", door.get.color) else show("+", door.get.darkColor)
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

  private def setPlayer(p : Player) : Unit =
    tileArray(p.pos.y)(p.pos.x) = "@"

  private def setEnemy(e : Enemy) : Unit =
    tileArray(e.pos.y)(e.pos.x) = show(e.symbol.toString, e.color)

  private def createHud(p : Player) : String = {
    val str = new StringBuilder
    str ++= "Knave\n"
    val healthColor = p.hp.toFloat / p.maxHp.toFloat match {
      case x if x >= 0.75 => "green"
      case x if x >= 0.25 => "yellow"
      case _ => "red"
    }
    str ++= s"Health: ${show(p.hp + "%", healthColor)}\n"
    str.toString
  }

  def displayFull(w : World) : Unit = {
    resetTileArray
    setDungeon(w.dungeon)
    setPlayer(w.player)
    for(e <- w.getEnemies) setEnemy(e)
    map.innerHTML = buildString + createHud(w.player)
  }

  def display(w : World) : Unit = {
    resetTileArray

    val fov = w.dungeon.fieldOfVisioin(w.player.pos, w.player.vision).toList
    w.dungeon.visitCoords(fov)

    setDungeonFov(w.dungeon, fov)
    setPlayer(w.player)
    for(e <- w.getEnemies.filter(e => fov.contains(e.pos))) setEnemy(e)
    map.innerHTML = buildString + createHud(w.player)
  }
}
