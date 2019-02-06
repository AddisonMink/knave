package knave.display
import knave.display.Palette.{orange, red}
import knave.world.World
import knave.world.dungeon.{Coord, Dungeon}
import knave.world.enemy.Enemy
import knave.world.dungeon.Size._
import knave.world.player.Player

object DisplayFov extends Display {

  private def setDungeon(d : Dungeon, p : Player) : Unit = {
    p.visitedTiles.foreach(setTile(d, _, false))
    p.fieldOfVision.foreach(setTile(d, _, true))
  }

  private def setEnemyFov(e : Enemy, w : World) : Unit =
    if(w.player.hidden)
      e.fieldOfVision.intersect(w.player.fieldOfVision).foreach(setTile(w.dungeon,_,true,None,Some(orange)))
    else
      w.dungeon.circle(e.pos, e.vision*2).intersect(w.player.fieldOfVision).foreach(setTile(w.dungeon,_,true,None,Some(red)))

  override def display(w : World, logs : List[String] = List(), speedRound : Boolean) : Unit = {
    super.display(w,logs,speedRound)
    setDungeon(w.dungeon, w.player)
    val enemies = w.getEnemies.filter(e => w.player.fieldOfVision.contains(e.pos))
    enemies.foreach(setEnemyFov(_,w))
    w.getItems.filter(i => w.player.fieldOfVision.contains(i.pos)).foreach(setItem(_))
    setPlayer(w.player)
    enemies.foreach(setEnemy(_,speedRound))
    log.innerHTML = createLog(logs)
    hud.innerHTML = createHud(w.player)
  }
}
