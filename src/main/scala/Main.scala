package proj

import java.awt.Dimension
import scala.swing._
import java.awt.Color

object Main {
  
  def main(args: Array[String]): Unit = {
    val model = new Model
    val textView = new TextView
    val controller = new Controller(model)
    controller.register(textView)
    textView.init(controller)
  }
}
class Model {    
  val po = new PlayerOrder( Array.fill(4)( new Personality0 ) )
  val game = new Game()
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
    // val input = readLine("waiting\n") 
  }

  def initGame = {
    model.initGame
    showGame
    // doGame
    doTurn
  }
  // initGame
  
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
  }

  // def advancePlayerOrder { 
  //   val result = model.advancePlayerOrder 
  //   views.foreach(_.displayPlayerOrder(result))
  // }

  // def checkForWinner(): Option[Player] = None
  // initGame
 
}