package proj
//smallchange
import util.Random
import io.Source

sealed trait Facing
case object North extends Facing
case object South extends Facing
case object East extends Facing
case object West extends Facing
class Robot(var x:Int,var y:Int,val robotNumber:Int,var direction:Facing, var hand:Array[Card]){
    private var currentFlag = 0
    def updateFlag(flag:Int){if(flag == currentFlag + 1) currentFlag+=1}
    def getFlag:Int = currentFlag
}
sealed trait CardType
case object Move1 extends CardType
case object Move2 extends CardType
case object Move3 extends CardType
case object RotateRight extends CardType
case object RotateLeft extends CardType
case object BackUp extends CardType
case object UTurn extends CardType
class Card(val attribute:CardType, val priority:Int)
class Deck{
    private var cards: List[Card] = List()
    private var priorities = Random.shuffle(1 to 84)
    def shuffle{cards = Random.shuffle(cards).toList}
    def genCards(t:CardType, num:Int){
        for(i<-1 to num){
            cards = new Card(t, priorities.head) +: cards
            priorities = priorities.tail
        } 
    }
    def init{
        // fill cards var with all cards then shuffle
        genCards(Move1, 18)
        genCards(Move2, 12)
        genCards(Move3, 6)
        genCards(BackUp, 6)
        genCards(RotateRight, 18)
        genCards(RotateLeft, 18)
        genCards(UTurn, 6)
        shuffle
    } 
    def draw:Card={
        val c = cards.head
        cards = cards.tail
        c
    }
    def replace(c:Card){cards=c+:cards}
    def length = cards.length
    init
}
class Register{
    private var cards = Array.fill[Card](5)(null)
    def updateRegister(newCards:Array[Card]) {if(newCards.length ==5) cards = newCards} //assuming benevolent input
    def viewRegister:Array[Card]= cards
}

class Game{
    private var board:Array[Array[Char]] = null //uninitialized
    private var numRobots = 4
    private var robots = Array.fill[Robot](numRobots)(null)
    private var deck = new Deck
    private var registers = Array.fill[Register](numRobots){ new Register }
    val startBlocks = List((5,15),(6,15),(3,14),(8,14))
    def decksize = deck.length
    def interpretBoard:Array[Array[Char]]={
        var r = Array.fill[Char](12,16)(' ')
        /*val source = scala.io.Source.fromFile("board.txt")
        val rows = source.getLines.toArray
        source.close()*/
        val rows = "* * * * * * * * * * * * * *\n* X X T U T D U T D T U X *\n* L X H U X D U $ D X X R *\n* [ X X U X D U X D [ X ] *\n* R R R X X D U X D L L L *\n* [ $ X X B D U B X X X ] *\n* L L L L L X X L L L L L *\n* X R R R R X X R R R R R *\n* [ X X X T D U T X $ X ] *\n* L L L X X D U X X L L L *\n* [ X X U X D U X D X T ] *\n* H B X U X D U X D X X R *\n* X X B U B D X B D B U X *\n* X X X X [ X X ] X X X X *\n* X X X X X X X X X X X X *\n* X X X 3 X X X X 4 X X X *\n* X X X X X 1 2 X X X X X *\n* * * * * * * * * * * * * *".split('\n')
        for(y<-1 to 16){
            val columns = rows(y).split(' ').map(_(0))
            for(x<-1 to 12) r(x-1)(y-1) = columns(x)
        }
        r
    }
    def init{
        //load board
        //set initial robot positions 
        //draw 7 cards for each robot's hand
        //update robots var
        board = interpretBoard
        for(i<-robots.indices){
            val cards = Array.fill(7){deck.draw}
            var start = startBlocks(i)
            robots(i) = new Robot(start._1, start._2, i+1, North, cards)
        }
    }
    def getLayout:Array[Array[Char]]={board.map(a => a)}
    def textGameArea:Array[Array[Char]]={
        val newBoard = Array.fill(board(0).size,board.size)(' ')
        for(x<-board.indices;y<-board(0).indices) newBoard(y)(x) = board(x)(y)
        val l = getRobotLocations
        for(p<-l.indices) newBoard(l(p)._2)(l(p)._1) = (p+1).toString()(0)
        newBoard
    }
    def getRobotLocations:Array[(Int,Int)]={robots.map(r => (r.x,r.y))}
    def offBoard(x:Int, y:Int):Boolean = {
        //check to see if (x,y) is a hole or off the board
        if(!board.indices.contains(x) || !board(0).indices.contains(y)) true
        else if(board(x)(y)=='H') true
        else false
    }
    def playCard(robotNumber:Int,card:Card){
        //check to see if move is valid, if so update robot(s) position 
        //check to see if robot is on a hole
        val r = robots(robotNumber-1)
        
        card.attribute match{
            case Move1 => {
                //must check for
                r.direction match{
                    case North => {
                        if(offBoard(r.x,r.y-1))  returnToStart(robotNumber)
                        else if(board(r.x)(r.y) != 'T' && board(r.x)(r.y-1) != 'B')
                            r.y -= 1
                    }
                    case South => {
                        if(offBoard(r.x,r.y+1))  returnToStart(robotNumber)
                        else if(board(r.x)(r.y+1) != 'T' && board(r.x)(r.y) != 'B')
                            r.y += 1
                    }
                    case East => {
                        if(offBoard(r.x+1,r.y))  returnToStart(robotNumber)
                        else if(board(r.x)(r.y) != ']' && board(r.x+1)(r.y) != '[')
                            r.x += 1
                    }
                    case West => {
                        if(offBoard(r.x-1,r.y))  returnToStart(robotNumber)
                        else if(board(r.x-1)(r.y) != ']' && board(r.x)(r.y) != '[')
                            r.x -= 1
                    }
                }
            }
            case Move2 => {
                playCard(robotNumber, new Card(Move1,0))
                playCard(robotNumber, new Card(Move1,0))
            }
            case Move3 => {
                playCard(robotNumber, new Card(Move2,0))
                playCard(robotNumber, new Card(Move1,0))
            }
            case RotateRight => {
                r.direction match{
                    case North => {r.direction=East}
                    case South => {r.direction=West}
                    case East => {r.direction=South}
                    case West => {r.direction=North}
                }
            }
            case RotateLeft => {
                r.direction match{
                    case North => {r.direction=West}
                    case South => {r.direction=East}
                    case East => {r.direction=North}
                    case West => {r.direction=South}
                }
            }
            case BackUp => {
                playCard(robotNumber, new Card(UTurn,0))
                playCard(robotNumber, new Card(Move1,0))
                playCard(robotNumber, new Card(UTurn,0))
            }
            case UTurn  => {
                r.direction match{
                    case North => {r.direction=South}
                    case South => {r.direction=North}
                    case East => {r.direction=West}
                    case West => {r.direction=East}
                }
            }
        }
    }
    def returnToStart(robotNumber:Int){
        robots(robotNumber-1).x = startBlocks(robotNumber-1)._1
        robots(robotNumber-1).y = startBlocks(robotNumber-1)._2
    }
    def drawCards{
        //5 new cards to each Robot.hand
        for(i<-robots.indices)
            for(j<-0 to 4) robots(i).hand = deck.draw +: robots(i).hand
    } 
    def replaceCards{
        //empty Register to Deck.replace for all robots
        for(i<-registers.indices)
            for(c<-registers(i).viewRegister) deck.replace(c)
    }
    def viewRegisters:Array[Register]={
        registers.map{r => 
            val a = new Register
            a.updateRegister(r.viewRegister)
            a
        }
    } 
    def updateRegister(robotNumber:Int, newReg:Register, newHand:Array[Card]){
        //this is used to place cards from players' hands into their registers
        //update correct register with a copy of reg and update Robot.hand
        registers(robotNumber-1) = newReg
        robots(robotNumber-1).hand = newHand
    } 
    def getScore:Array[Int]={robots.map(r=>r.getFlag)}
    def getOrder(registerPhase:Int):List[Int]={
        //sort Register.viewRegister(registerPhase).priority
        registers.map(_.viewRegister(registerPhase).priority).zipWithIndex.sortBy(_._1).map(_._2+1).toList
    } 
    def endRegisterPhase{
        //move conveyer belts
        //conveyer belts are R L U D
        for(r<-robots){
            val robotNumber = r.robotNumber
            board(r.x)(r.y) match{
                case 'R' => {
                    if(offBoard(r.x+1,r.y)) returnToStart(robotNumber)
                    else if(board(r.x+1)(r.y) != '[')
                        r.x = r.x+1
                }
                case 'L' => {
                    if(offBoard(r.x-1,r.y)) returnToStart(robotNumber)
                    else if(board(r.x-1)(r.y) != ']')
                        r.x = r.x-1
                }
                case 'U' => {
                    if(offBoard(r.x,r.y-1)) returnToStart(robotNumber)
                    else if(board(r.x)(r.y-1) != 'B')
                        r.y = r.y-1
                }
                case 'D' => {
                    if(offBoard(r.x,r.y+1)) returnToStart(robotNumber)
                    else if(board(r.x)(r.y+1) != 'T')
                        r.y = r.y+1
                }
                case _ => { }
            }
        }
    }
    def endTurn{
        //replace cards and shuffle deck then draw new cardsB
        replaceCards
        deck.shuffle
        drawCards
    }
    def showHand(robotNumber:Int):Array[Card] = robots(robotNumber-1).hand
    init
}

class GameSim{
    private val players = Array.fill(4){ new Personality0 }
    private val board = new Game
    private var phase:List[Int] = (0 until 5).toList
    private var playerup:List[Int] = (1 to players.length).toList
    def curPhase:Int = {
        val t = phase.head
        phase = phase.tail :+ t
        t
    }
    def curUp:Int={
        val t = playerup.head
        playerup = playerup.tail :+ t
        t
    } 
    def playArea:Array[Array[Char]]={board.textGameArea}
    def printPlayArea{
        for(l<-playArea) {
            var tl = ""
            for(c<-l) tl+= c + " "
            println(tl)
        }
    }
    def playOrder(phaseNumber:Int):List[Int]={board.getOrder(phaseNumber)}
    def init{board.init}
    def checkWin:Int={ //playernum if player wins else 0
        val s = board.getScore
        for(i<-s.indices) if(s(i)==3) return i
        0
    }
    def doRegisterPhase(disp:Boolean =false){
        val currentPhase = curPhase
        val r = board.viewRegisters
        for(p<-playOrder(currentPhase)){
            val card = r(p-1).viewRegister(currentPhase)
            board.playCard(p, card)
            if(disp) {
                println("player "+p.toString+" executes "+card.attribute.toString)
                printPlayArea
                val input = readLine("enter to continue, esc to quit\n")
                if (input=="esc") throw new Exception("esc")
            }
        }
        board.endRegisterPhase
    }
    def doMove(disp:Boolean = false){
        val player = curUp
        val selection = players(player-1).placeCards(player, board.showHand(player), board.getLayout, board.getRobotLocations)
        assert(selection.toSet == board.showHand(player).toSet) // no cheating 
        val r = new Register
        r.updateRegister(selection.take(5))
        board.updateRegister(player, r, selection.drop(5))
        if(disp){
            var s = "player "+player.toString+" plays"
            for(i<-selection.dropRight(2)) s+= " "+i.attribute.toString
            println(s)
        }
    }
    def doTurn(disp:Boolean = false){
        //get card selections from players
        for(i<-players.indices)
            doMove(disp)
        for(currentPhase<-0 until 5)
            doRegisterPhase(disp)
        board.endTurn
        if(disp) println("Turn complete")
    }
    def doGame(disp:Boolean = false){
        init
        while(checkWin==0) doTurn(disp)
        if(disp) println("winner is "+ checkWin.toString)
    }
}

abstract class Personality{
    //sort the cards in your hand by priority (first 5 will be placed in the register)
    def placeCards(robotNumber:Int,hand:Array[Card],board:Array[Array[Char]],positions:Array[(Int,Int)]):Array[Card]
}
class Personality0 extends Personality{
    def placeCards(robotNumber:Int,hand:Array[Card],board:Array[Array[Char]],positions:Array[(Int,Int)]):Array[Card] = {
        hand // really lame personality
    }
}
