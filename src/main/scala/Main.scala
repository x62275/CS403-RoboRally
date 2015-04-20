package proj

import java.awt.Dimension
import scala.swing._
import java.awt.Color

object Main {
  
  def main(args: Array[String]): Unit = {
    // val model = new Model
    // val textView = new TextView
    // val controller = new Controller(model)
    // controller.register(textView)
    // textView.init(controller)
    val po = new PlayerOrder( Array.fill(4)( new Personality0 ) )
    val game = new GameSim( new Game , po)
    game.doGame
  }
}
/*
class Model {
  private val players
  private val board = new Game
}

class Controller(model: Model) extends GameSim //(model: Model) extends DM(model.playerOrder, model.tcd, model.dcd, model.board) {
  var views: Set[View] = Set()
  
  def register(view: View) {
    views += view
  }

  def initGame = {
    model.initGame
    showGame
  }

  def showGame = {
    showPlayerOrder
    showPlayingArea
    updateMenu
  }

  def updateMenu {
    views.foreach(_.updateMenu)
  }

  def showPlayerOrder { 
    val result = model.showPlayerOrder 
    views.foreach(_.displayPlayerOrder(result))
  }

  def showMove {
    ???
  }

  def showExecCard {
    ???
  }

  def showPlayingArea {
    views.foreach(_.displayPlayingArea(model.board, model.tcd))
  }

  def advancePlayerOrder { 
    val result = model.advancePlayerOrder 
    views.foreach(_.displayPlayerOrder(result))
  }

  def checkForWinner(): Option[Player] = None
  initGame
 
}
*/