package proj

import java.awt.Dimension
import scala.swing._
import java.awt.Color

object Main {
  
  def main(args: Array[String]): Unit = {
    val model = new Model
    // val textView = new TextView
    val controller = new Controller(model)
    // controller.register(textView)
    // textView.init(controller)
  }
}
class Model {    
  val po = new PlayerOrder( Array.fill(4)( new Personality0 ) )
  val game = new Game
  def initGame { game.init }
}

class Controller(model: Model) extends GameSim(model.game, model.po) {
  /*var views: Set[View] = Set()
  
  def register(view: View) {
    views += view
  }*/
  def printPlayArea{
        for(l<-model.game.textGameArea) {
            var tl = ""
            for(c<-l) tl+= c + " "
            println(tl)
        }
    }
  override def doExecute(p:Int, currentPhase:Int):Card = {
    val card = super.doExecute(p, currentPhase)
    showCardExec("player " + p.toString + " executes " + card.attribute.toString)
    card
  }
  def showCardExec(text_to_update:String) { 
    println(text_to_update)
    printPlayArea
    val input = readLine("waiting\n") 
  }
  def initGame = {
    model.initGame
    //showGame
    doGame
  }
  initGame
  // def showGame = {
  //   showPlayerOrder
  //   showPlayingArea
  //   updateMenu
  // }

  // def updateMenu {
  //   views.foreach(_.updateMenu)
  // }

  // def showPlayerOrder { 
  //   val result = model.showPlayerOrder 
  //   views.foreach(_.displayPlayerOrder(result))
  // }

  // def showMove {
  //   ???
  // }

  // def showExecCard {
  //   ???
  // }

  // def showPlayingArea {
  //   views.foreach(_.displayPlayingArea(model.board, model.tcd))
  // }

  // def advancePlayerOrder { 
  //   val result = model.advancePlayerOrder 
  //   views.foreach(_.displayPlayerOrder(result))
  // }

  // def checkForWinner(): Option[Player] = None
  // initGame
 
}