package proj

// import java.awt.Dimension
import scala.swing._
import javax.swing.ImageIcon
import java.awt.{Color, Image}
import java.io.File
import scala.collection.mutable.Map

abstract class View {
  var controller: Option[Controller] = None
  val frame = new MainFrame
  // val playerOrderDisplay: Component
  val playingAreaDisplay: Component

  def init(ctrl: Controller) {
    controller = Some(ctrl)

    frame.title = "Robo Rally"
    frame.menuBar = createMenu
    frame.contents = new BoxPanel(Orientation.Vertical) {
      // contents += playerOrderDisplay
      contents += playingAreaDisplay
    }
    frame.size = new Dimension(600, 600)
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
        // contents += new MenuItem(Action("Player Order") {
        //   controller.get.showPlayerOrder
        // })
        contents += new MenuItem(Action("Playing Area") {
          controller.get.showPlayingArea
        })
      }
      
      contents += new Menu("Game") {
        contents += new MenuItem(Action("Do Turn") {
          controller.get.doTurn
          controller.get.showPlayingArea
        })
        contents += new MenuItem(Action("Do Move") {
          controller.get.doMove
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

  // def displayPlayerOrder(players: PlayerOrder)
  def displayCardExec(text: String)
  def displayPlayingArea(game: Game)
}

class TextView extends View {
  // val playerOrderDisplay = new TextArea(1, 1) { background = awt.Color.yellow }
  val playingAreaDisplay = new TextArea(100, 1) { background = Color.white }

  // def displayPlayerOrder(players: PlayerOrder) {
  //   // playerOrderDisplay.text = players.show
  //   playerOrderDisplay.text = "Name/Trains\tScore\tTrain & Destination Cards"
  //   for (p <- players) {
  //     playerOrderDisplay.text += "\n\n"
  //     playerOrderDisplay.text += p.color.toString + "/" + p.count +"\t"
  //     playerOrderDisplay.text += p.getScore.toString + "\t"
  //     playerOrderDisplay.text += p.trainCards.mkString(",") + "\n\t\t"
  //     playerOrderDisplay.text += p.dstCards.mkString(",")
  //   }
  //   // playerOrderDisplay.text = playerOrderDisplay.text.tail
  // }

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
  val dir = new File("img")
  for (img <- dir.listFiles.map(_.toString)) {
    images(img.slice(4,img.length - 4)) = new ImageIcon(img).getImage.getScaledInstance(25,25,Image.SCALE_DEFAULT)
  }

  val labels = Array.fill(16*12)(genLabel(images("open")))
  val len = 16
  val width = 12
  val playingAreaDisplay = new GridPanel(width, len) {
    val d = new Dimension(30*width, 30*len)
    preferredSize = d
    maximumSize = d
    minimumSize = d
    background = Color.cyan
    // for (i <- 1 to 16*12) contents += genLabel(images("open"))
    labels.foreach(contents += _)
  }
  def genLabel(img: Image) = {
    new Label {
      val d = new Dimension(30,30)
      preferredSize = d
      maximumSize = d
      minimumSize = d
      verticalAlignment = Alignment.Top
      horizontalAlignment = Alignment.Left
      val imageIcon = new ImageIcon(img)
      icon = imageIcon
    }
  }

  def grabImage(c: Char): Image = c match {
    case 'X' => images("open")
    case '$' => images("flag_1")
    case 'H' => images("hole")

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


    case _ => images("red_home")
  }
  
  def displayCardExec(text: String) { }
  def displayPlayingArea(game: Game) {
    val b = game.textGameArea.flatten
    for (i <- b.indices) {
      labels(i).imageIcon.setImage(grabImage(b(i)))
      playingAreaDisplay.repaint()
    }
  }
}