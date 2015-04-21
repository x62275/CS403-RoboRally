package proj

object Main {
  
  def main(args: Array[String]): Unit = {
    val model = new Model
    // val textView = new TextView
    val gui = new GUI
    val controller = new Controller(model)
    controller.register(gui)
    gui.init(controller)
  }
}
class Model {    
  val ai:Array[Personality] = Array( new Loves_Conveyer_Belts , new Personality0 , new Eyes_Closed , new Shortest_Path )
  //val ai = Array.fill[Personality](4)( new Eyes_Closed )
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
  
  override def doExecute(p:Int, currentPhase:Int):Card = {
    val card = super.doExecute(p, currentPhase)
    showCardExec("player " + p.toString + " executes " + card.attribute.toString)
    card
  }

  // override def doMove {
  //   super.doMove
  //   showPlayingArea
  // }

  // override def doTurn {
  //   super.doTurn
  //   showPlayingArea
  // }

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
    // showPlayerOrder
    showPlayingArea
  }

  // def showPlayerOrder { 
  //   val result = model.showPlayerOrder 
  //   views.foreach(_.displayPlayerOrder(result))
  // }

  def showPlayingArea {
    views.foreach(_.displayPlayingArea(model.game))
    // for(l<-model.game.textGameArea) {
    //  var tl = ""
    //  for(c<-l) tl+= c + " "
    //  println(tl)
    // }
  }

  // def advancePlayerOrder { 
  //   val result = model.advancePlayerOrder 
  //   views.foreach(_.displayPlayerOrder(result))
  // }

  // def checkForWinner(): Option[Player] = None
  // initGame
 
}