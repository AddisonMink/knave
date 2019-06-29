package knave.display
import knave.display.Palette.{orange, red}
import knave.world.World
import knave.world.dungeon.Dungeon
import knave.world.dungeon.Dungeon._
import knave.world.enemy.{Alerted, Enemy}
import knave.world.player.Player

object DisplayFov extends Display {

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

  override def display(w : World, speedRound : Boolean, prompt: String = "") : Unit = {
    super.display(w,speedRound,prompt)
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
}
