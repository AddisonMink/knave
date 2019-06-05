package knave.display

import knave.world.{World}
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.dungeon.Dungeon._
import knave.world.dungeon.Size.{height, width}
import knave.world.enemy.Enemy
import Palette._

object DisplayFull extends Display {

  private def setDungeon(d : Dungeon) : Unit =
    for(y <- 0 until height)
      for(x <- 0 until width)
        if((Coord(x,y) +: Coord(x,y).adjacent).filter(_.inBounds).exists(d.isFloor(_)))
          setTile(d, Coord(x,y),true)

  private def setEnemyFov(e : Enemy, w : World) : Unit =
    if(w.player.hidden)
      e.fieldOfVision.foreach(setTile(w.dungeon,_,true,Some(orange)))
    else {
      import w.dungeon
      e.pos.visibleDisk(e.vision*2).foreach(setTile(w.dungeon,_,true,Some(red)))
    }

  override def display(w : World, logs : List[String] = List(), speedRound : Boolean) : Unit = {
    super.display(w,logs,speedRound)
    setDungeon(w.dungeon)
    w.getItems.foreach(setItem(_))
    setPlayer(w.player)
    val enemies = w.getEnemies
    enemies.foreach(setEnemyFov(_,w))
    enemies.foreach(setEnemy(_,speedRound))
    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }
}
