package proj

import scala.swing._
import javax.swing.ImageIcon
import java.awt.{Color, Image}
import java.io.File
import scala.collection.mutable.Map

abstract class View {
  var controller: Option[Controller] = None
  val frame = new MainFrame
  val cardExecDisplay: Component
  val playingAreaDisplay: Component

  def init(ctrl: Controller) {
    controller = Some(ctrl)

    frame.title = "Robo Rally"
    frame.menuBar = createMenu
    frame.contents = new BoxPanel(Orientation.Horizontal) {
      contents += playingAreaDisplay
      contents += cardExecDisplay
    }
    frame.size = new Dimension(900, 800)
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
          controller.get.initGame()
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
      contents += new Menu("Board") {
        contents += new MenuItem(Action("Default") {
          controller.get.initGame(0)
        })
        contents += new MenuItem(Action("Danger Zone!!!!") {
          controller.get.initGame(1)
        })
        contents += new MenuItem(Action("Blank Space") {
          controller.get.initGame(2)
        })
        contents += new MenuItem(Action("Rama dama ding dong") {
          controller.get.initGame(99)
        })
      }
    }
  }

  def displayCardExec(text: String)
  def displayPlayingArea(game: Game)
}

class GUI extends View {
  val lw = 40
  val images = loadImages()

  val labels = Array.fill(16*12)(genLabel(images("open")))
  val len = 16
  val width = 12

  val cardExecDisplay = new TextArea(10, 1) {
    val d = new Dimension(lw*width, lw*len)
    preferredSize = d
    maximumSize = d
    minimumSize = d
    background = Color.cyan
    editable = false
    font = new Font("ComicSans",0,16)
  }
  val playingAreaDisplay = new GridPanel(len, width) {
    val d = new Dimension(lw*width, lw*len)
    preferredSize = d
    maximumSize = d
    minimumSize = d
    background = Color.cyan
    labels.foreach(contents += _)
  }

  def loadImages(dir: File = new File("img")): Map[String,Image] = {
    val images: Map[String,Image] = Map.empty
    try {
      for (img <- dir.listFiles.map(_.toString)) {
        images(img.slice(4,img.length - 4)) = new ImageIcon(img).getImage.getScaledInstance(lw,lw,Image.SCALE_SMOOTH)
      }
      images
    } catch { case _:Throwable => loadImages(dir) }

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

  def grabImage(c: Char, facing: String = ""): Image = c match {
    case 'X' => images("open")
    case 'H' => images("hole")

    case '$' => images("flag_1")
    case '%' => images("flag_2")
    case '&' => images("flag_3")

    case '1' => images("green_robot" + "_" + facing)
    case 'g' => images("green_home")
    case '2' => images("blue_robot" + "_" + facing)
    case 'b' => images("blue_home")
    case '3' => images("red_robot" + "_" + facing)
    case 'r' => images("red_home")
    case '4' => images("purple_robot" + "_" + facing)
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
  
  var ceLen = 1
  var cardExecHistory: List[String] = List.empty
  def displayCardExec(text: String) {
    cardExecHistory +:= text
    ceLen += 1
    if (ceLen%5==0) { ceLen += 1; cardExecHistory +:= "_"*20 }
    cardExecDisplay.text = cardExecHistory.take(25).mkString("\n")
    // cardExecDisplay.text = text + "\n" + cardExecDisplay.text
  }

  val previousBoard = Array.fill(16*12)('X')
  def displayPlayingArea(game: Game) {
    val b = game.textGameArea.flatten
    for (i <- b.indices) {
      b(i) match {
        case '1' => { labels(i).imageIcon.setImage(grabImage(b(i),game.robots(0).direction.toString)); previousBoard(i) = b(i) }
        case '2' => { labels(i).imageIcon.setImage(grabImage(b(i),game.robots(1).direction.toString)); previousBoard(i) = b(i) }
        case '3' => { labels(i).imageIcon.setImage(grabImage(b(i),game.robots(2).direction.toString)); previousBoard(i) = b(i) }
        case '4' => { labels(i).imageIcon.setImage(grabImage(b(i),game.robots(3).direction.toString)); previousBoard(i) = b(i) }
        case _ => if (b(i) != previousBoard(i)) { labels(i).imageIcon.setImage(grabImage(b(i))); previousBoard(i) = b(i) }
      }
    }
    playingAreaDisplay.repaint()
  }
}