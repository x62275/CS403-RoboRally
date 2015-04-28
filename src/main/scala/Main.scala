package proj

object Main {
  
  def main(args: Array[String]): Unit = {
    val model = new Model
    val gui = new GUI
    val controller = new Controller(model)
    controller.register(gui)
    gui.init(controller)
  }
}
class Model {    
  val ai:Array[Personality] = Array( new Lazy_Leo , new Greedy_George , new Ray_Charles , new Bobby_Fischer )
  //val ai = Array.fill[Personality](4)( new Greedy_George )
  val po = new PlayerOrder( ai )
  val game = new Game()
  game.init
  def initGame { game.init }
}

class Controller(model: Model) extends GameSim(model.game, model.po) {
  var views: Set[View] = Set()
  
  def register(view: View) {
    views += view
  }
  
  override def doExecute(p:Int, currentPhase:Int):String = {
    val text = super.doExecute(p, currentPhase)
    showCardExec(text)
    Thread sleep 100
    text
  }
  def showCardExec(text_to_update:String) {
    showPlayingArea
    views.foreach(_.displayCardExec(text_to_update))
    //val input = readLine("waiting\n") 
  }

  def initGame = {
    model.initGame
    showGame
  }
  
  def showGame = {
    showPlayingArea
  }

  def showPlayingArea {
    views.foreach(_.displayPlayingArea(model.game))
  } 
}