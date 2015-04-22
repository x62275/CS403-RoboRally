package proj

import scala.swing._
import javax.swing.ImageIcon
import java.awt.{Color, Image}
import java.io.File
import scala.collection.mutable.Map

abstract class View {
  var controller: Option[Controller] = None
  val frame = new MainFrame
  val playingAreaDisplay: Component

  def init(ctrl: Controller) {
    controller = Some(ctrl)

    frame.title = "Robo Rally"
    frame.menuBar = createMenu
    frame.contents = new BoxPanel(Orientation.Vertical) {
      contents += playingAreaDisplay
    }
    frame.size = new Dimension(600, 800)
    frame.centerOnScreen
    frame.visible = true
  }

  def updateMenu { frame.menuBar = createMenu }

  protected def createMenu = {
    new MenuBar {
      contents += new Menu("Show") {
        contents += new MenuItem(Action("All") {
          controller.get.showGame
        })
        contents += new MenuItem(Action("Playing Area") {
          controller.get.showPlayingArea
        })
      }
      
      contents += new Menu("Game") {
        contents += new MenuItem(Action("Do Turn") {
          controller.get.doTurn
          controller.get.showPlayingArea
        })
        contents += new MenuItem(Action("Do Game") {
          controller.get.doGame
          controller.get.showPlayingArea
        })
        contents += new MenuItem(Action("Restart") {
          controller.get.initGame
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
  }

  def displayCardExec(text: String)
  def displayPlayingArea(game: Game)
}

class TextView extends View {
  val playingAreaDisplay = new TextArea(100, 1) { background = Color.white }

  def displayPlayingArea(game: Game) {
    playingAreaDisplay.text = ""
    for(l<-game.textGameArea) {
      var tl = ""
      for(c<-l) tl+= c + " "
      playingAreaDisplay.text += tl + "\n"
    }
  }

  def displayCardExec(text: String) {
    playingAreaDisplay.text = text + "\n" + playingAreaDisplay
  }
}

class GUI extends View {
  val images: Map[String,Image] = Map.empty
  //load images
  val lw = 40
  val dir = new File("img")
  for (img <- dir.listFiles.map(_.toString)) {
    images(img.slice(4,img.length - 4)) = new ImageIcon(img).getImage.getScaledInstance(lw,lw,Image.SCALE_SMOOTH)
  }

  val labels = Array.fill(16*12)(genLabel(images("open")))
  val len = 16
  val width = 12
  val playingAreaDisplay = new GridPanel(len, width) {
    val d = new Dimension(lw*width, lw*len)
    preferredSize = d
    maximumSize = d
    minimumSize = d
    background = Color.cyan
    labels.foreach(contents += _)
  }
  def genLabel(img: Image) = {
    new Label {
      val d = new Dimension(lw,lw)
      preferredSize = d
      maximumSize = d
      minimumSize = d
      verticalAlignment = Alignment.Top
      horizontalAlignment = Alignment.Left
      val imageIcon = new ImageIcon(img)
      icon = imageIcon
    }
  }

  override def init(ctrl: Controller) {
    super.init(ctrl)
    ctrl.showPlayingArea
  }

  def grabImage(c: Char): Image = c match {
    case 'X' => images("open")
    case 'H' => images("hole")

    case '$' => images("flag_1")
    case '%' => images("flag_2")
    case '&' => images("flag_3")

    case '1' => images("green_robot")
    case 'g' => images("green_home")
    case '2' => images("blue_robot")
    case 'b' => images("blue_home")
    case '3' => images("red_robot")
    case 'r' => images("red_home")
    case '4' => images("purple_robot")
    case 'p' => images("purple_home")

    case 'L' => images("left_conveyer")
    case 'R' => images("right_conveyer")
    case 'U' => images("up_conveyer")
    case 'D' => images("down_conveyer")

    case '[' => images("left_wall")
    case ']' => images("right_wall")
    case 'T' => images("top_wall")
    case 'B' => images("bottom_wall")
  }
  
  def displayCardExec(text: String) { }
  def displayPlayingArea(game: Game) {
    val b = game.textGameArea.flatten
    for (i <- b.indices) {
      //labels(i) = genLabel(grabImage(b(i)))
      labels(i).imageIcon.setImage(grabImage(b(i)))
      playingAreaDisplay.repaint();
    }
  }
}