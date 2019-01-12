package knave.display

import knave.world.World
import org.scalajs.dom.document
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Size.{height, width}
import knave.world.player.Player

object Display {

  private val map = document.getElementById("map")

  private val tileArray = Array.ofDim[String](height,width)
  private def resetTileArray : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        tileArray(y)(x) = " "
  resetTileArray

  private def show(symbol : String, color : String) : String =
    "<span style=\"color : " + color + "\">" + symbol + "</span>"

  private def setDungeon(d : Dungeon) : Unit = {
    for(y <- 0 until height)
      for(x <- 0 until width) {
        val c = Coord(x,y)
        lazy val floor = d.floorAt(c)
        lazy val wall = d.wallAt(c)
        lazy val door = d.doorAt(c)
        if(floor.isDefined) tileArray(y)(x) = show(".", floor.get.color)
        else if(wall.isDefined) tileArray(y)(x) = show("#", wall.get.color)
        else if(door.isDefined) tileArray(y)(x) = if (door.get.open) show("/", door.get.color) else show("+", door.get.color)
      }
  }

  private def setPlayer(p : Player) : Unit =
    tileArray(p.pos.y)(p.pos.x) = "@"

  def display(w : World) = {
    resetTileArray
    setDungeon(w.dungeon)
    setPlayer(w.player)
    val str = new StringBuilder
    for(y <- 0 until height) {
      for (x <- 0 until width)
        str ++= tileArray(y)(x)
      str += '\n'
    }
    map.innerHTML = str.toString
  }
}
