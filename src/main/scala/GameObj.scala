package proj

import util.Random
import io.Source

sealed trait Facing
case object North extends Facing
case object South extends Facing
case object East extends Facing
case object West extends Facing
case class Robot(var x:Int,var y:Int,val robotNumber:Int,var direction:Facing, var hand:Array[Card], var currentFlag:Int = 0){
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
class Card(val attribute:CardType, val priority:Int){
    override def toString = attribute.toString
}
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

class Game( var board:Array[Array[Char]] = null, var robots:Array[Robot] = Array.fill[Robot](4)(null)) {
    //private var board:Array[Array[Char]] = null //uninitialized
    def copy = new Game(board.map(_.clone), robots.map(_.copy()))
    private var deck = new Deck
    private var registers = Array.fill[Register](4){ new Register }
    val startBlocks = Array((5,15),(6,15),(3,14),(8,14))
    val flags = Array((9,7),(1,4),(8,1))
    def decksize = deck.length
    def interpretBoard(i:Int = 0):Array[Array[Char]]={
        var r = Array.fill[Char](12,16)(' ')
        /*val source = scala.io.Source.fromFile("board.txt")
        val rows = source.getLines.toArray
        source.close()*/
        val rowslist =  List("* * * * * * * * * * * * * *\n* X X T U T D U T D T U X *\n* L X H U X D U & D X X R *\n* [ X X U X D U X D [ X ] *\n* R R R X X D U X D L L L *\n* [ % X X B D U B X X X ] *\n* L L L L L X X L L L L L *\n* X R R R R X X R R R R R *\n* [ X X X T D U T X $ X ] *\n* L L L X X D U X X L L L *\n* [ X X U X D U X D X T ] *\n* H B X U X D U X D X X R *\n* X X B U B D X B D B U X *\n* X X X X [ X X ] X X X X *\n* X X X X X X X X X X X X *\n* X X X r X X X X p X X X *\n* X X X X X g b X X X X X *\n* * * * * * * * * * * * * *".split('\n'),
                             "* * * * * * * * * * * * * *\n* H L U L U L U L U L U H *\n* R D R D R D R & R D R D *\n* U R U R U R U R U R D L *\n* R U R H R D R D H D R D *\n* U % U L U L U L U L D L *\n* R U R D R H H D R D R D *\n* U L U L U H H L U L D L *\n* R U R D R D R D R $ R D *\n* U L U H U L U L H L D L *\n* R U L D L D L D L D L D *\n* U L U L U L U L U L D L *\n* H D R D R D R D R D R H *\n* T T T [ X X X X ] T T T *\n* X X ] X [ ] [ ] X [ X X *\n* X X ] r [ ] [ ] p [ X X *\n* X X ] X [ g b ] X [ X X *\n* * * * * * * * * * * * * *".split('\n')
                             )
        var rows = rowslist(i)
        for(y<-1 to 16){
            val columns = rows(y).split(' ').map(_(0))
            for(x<-1 to 12) r(x-1)(y-1) = columns(x)
        }
        r
    }
    def init(i:Int = 0){
        //load board
        //set initial robot positions 
        //draw 7 cards for each robot's hand
        //update robots var
        board = interpretBoard(i)
        deck = new Deck
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
                case '$' => r.updateFlag(1)
                case '%' => r.updateFlag(2)
                case '&' => r.updateFlag(3)
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
    private var phase:List[Int] = (0 until 5).toList
    def curPhase:Int = {
        val t = phase.head
        phase = phase.tail :+ t
        t
    }
}
case class PlayerOrder(players:Array[Personality]){
    assert(players.length == 4)
    private var playerup:List[Int] = (1 to players.length).toList
    def curUp:Int={
        val t = playerup.head
        playerup = playerup.tail :+ t
        t
    } 
}

class GameSim(board:Game, po:PlayerOrder){
    def playOrder(phaseNumber:Int):List[Int]={board.getOrder(phaseNumber)}
    def checkWin:Int={ //playernum if player wins else 0
        val s = board.getScore
        for(i<-s.indices) if(s(i)==3) return i+1
        0
    }
    
    def doExecute(p:Int, currentPhase:Int):String = {
        val r = board.viewRegisters
        val card = r(p-1).viewRegister(currentPhase)
        board.playCard(p, card)
        val word = p match {
            case 1 => "green"
            case 2 => "blue"
            case 3 => "red"
            case _ => "purple"
        }
        val s = word + " executes " + card.attribute.toString
        // println(s)
        s
    }
    def doRegisterPhase{
        val currentPhase = board.curPhase
        for(p<-playOrder(currentPhase)) doExecute(p, currentPhase)
        board.endRegisterPhase
    }
    def doMove{
        val player = po.curUp
        val selection = po.players(player-1).placeCards(player, board.showHand(player), board.copy)
        if(selection.toSet != board.showHand(player).toSet){// no cheating 
            // val e = selection.toSet
            // val e2 = board.showHand(player).toSet
            // val e3 = (e union e2) -- (e intersect e2).toList
            // this.synchronized{
            //     println("-----------CHEATING--------")
            //     println(e.map(_.attribute.toString).mkString(", "))
            //     println(e2.map(_.attribute.toString).mkString(", "))
            //     println(e3.map(_.attribute.toString).mkString(", "))
            //     println("---------------------------")
            // }
            throw new Exception("cheating")
        }
        val r = new Register
        r.updateRegister(selection.take(5))
        board.updateRegister(player, r, selection.drop(5))
        var s = "player "+player.toString+" chooses"
        for(i<-selection.dropRight(2)) s+= " "+i.attribute.toString
        // println(s)
    }
    def turnmotions{
        for(i<-po.players.indices)
            doMove
        for(currentPhase<-0 until 5)
            doRegisterPhase
        board.endTurn
    }
    def doTurn{
        //get card selections from players
        val t = new Thread(new Runnable() {
            def run() {
                turnmotions
            }
        });
        t.start();
    }
    def gameMotions:Int = {
        var turnNum = 0
        //board.init
        while(checkWin==0 && turnNum < 100) {
            turnmotions
            turnNum +=1
        }
        // println(checkWin.toString + " Wins")
        turnNum
    }
    def doGame{
        val t = new Thread(new Runnable() {
            def run() {
                gameMotions
            }
        });
        t.start();
    }
}

class QuickSim(model:Model, spawner:Spawner) extends GameSim(model.game, model.po) {
    override def gameMotions:Int = {
        val i = super.gameMotions
        spawner.addElem(i)
        i
    }

}
class Spawner(val n:Int = 10){
    var l:List[Int] = List()
    def addElem(i:Int){ l +:= i }
    def motions {
        for(i<- 1 to n) {
            val m = new Model
            val controller = new QuickSim(m, this)
            controller.doGame
        }
        while(l.length < n){
            Thread.sleep(1000)
        }
        println(l.mkString(", "))  
        val mean = l.sum / n
        val devs = l.map(s => (s-mean) * (s-mean))
        val stdev = Math.sqrt(devs.sum / n).toInt
        println(mean, stdev)
    }
    motions
}

abstract class Personality{
    //sort the cards in your hand by priority (first 5 will be placed in the register)
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card]
}
class Personality0 extends Personality{
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        hand // really lame personality
    }
}
class Lazy_Leo extends Personality{
    //loves conveyer belts
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        var localgame = game.copy
        //find my position
        def findpos(t:Game=localgame):(Int, Int) = (t.robots(robotNumber-1).x, t.robots(robotNumber-1).y)
        def findface(t:Game=localgame):Facing = t.robots(robotNumber-1).direction
        //find the flag
        val lookingfor = game.robots(robotNumber-1).getFlag
        val (x, y) = game.flags(lookingfor)
        //find the general direction of the flag
        def findangle(xn:Int, yn:Int) = {
            var angle = Math.toDegrees(Math.atan2(x - xn, y - yn));
            angle = angle + Math.ceil( -angle / 360 ) * 360
            if(45 <= angle && angle < 135) East
            else if(135 <= angle && angle < 225) South
            else if(225 <= angle && angle < 315) West
            else North
        }
        def distance(xn:Int, yn:Int) = {
            val dx = x - xn
            val dy = x - yn
            Math.sqrt(dx*dx + dy*dy)
        }
        //where will my cards take me?
        var localhand = hand
        val chosen:Array[Card] = Array.fill(7)(null)
        for(i<- 0 until 7){
            //println(chosen.map(a=> if(a==null) "n" else a.attribute.toString).mkString(", "))
            val choices1 = localhand.map{card => 
                val (x0, y0) = findpos()
                val testgame = localgame.copy
                testgame.playCard(robotNumber, card)
                testgame.endRegisterPhase
                val (x, y) = findpos(testgame)
                val c = game.board(x)(y)
                //println(card.attribute.toString, c=='U', y0 > y, findface(testgame)==North && findface()!=North,findface(testgame), c=='R' || c=='L' || c=='D', y0==y)
                //give the conveyer belt pieces a higher priority
                if(testgame.robots(robotNumber-1).getFlag > lookingfor) (0, card)
                //give cards that move upward next highest
                else if (distance(x0, y0) > distance(x, y)) (1, card)
                //if facing north, but werent before
                else if (findface(testgame)==findangle(x,y)) (2, card)
                //we would rather ride the free train then random stuff
                else if(c=='U' || c=='R' || c=='L' || c=='D') (3, card) 
                //at least dont go backwards
                else if(y0==y) (4, card)
                else (5, card)
            }.sortBy(_._1)
            //println(choices1.map(a=>a._1.toString+", "+a._2.attribute.toString).mkString(", "))
            val choices = choices1.map(_._2)
            //println(choices.map(_.attribute.toString).mkString(", "))
            chosen(i) = choices.head 
            //println("chose: " + choices.head.attribute.toString)
            localgame.playCard(robotNumber, choices.head)
            localhand = (localhand.toSet - choices.head).toArray
        }
        chosen
    }
    def test{
        val robotNumber = 1
        val testgame = new Game
        testgame.init()
        def play{
            val deck = new Deck
            val hand = Array.fill(7)(deck.draw)
            val choices = placeCards(robotNumber,hand, testgame.copy)
            // println(choices.map(_.attribute.toString).mkString(", "))
            for(i<-0 until 5) {
                testgame.playCard(robotNumber, choices(i))
                testgame.endRegisterPhase
            }
        }
        play
        play
    }
}

class Ray_Charles extends Personality {
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        //close your eyes and pick the cards
        Random.shuffle(hand.toList).toArray
    }
}

class Bobby_Fischer extends Personality {
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        def facing(t:Game=game) = t.robots(robotNumber-1).direction
        def position(t:Game=game):(Int, Int) = (t.robots(robotNumber-1).x, t.robots(robotNumber-1).y)
        //decide what flag you're on
        val lookingfor = game.robots(robotNumber-1).getFlag
        // println(robotNumber.toString+" is looking for flag "+lookingfor.toString)
        //find starting position
        val (x0:Int, y0:Int) = position()
        //find the flag
        val (x, y) = game.flags(lookingfor)
        //find the general direction of the flag
        def findangle(xn:Int=x0, yn:Int=y0) = {
            var angle = Math.toDegrees(Math.atan2(xn - x, yn - y));
            angle = angle + Math.ceil( -angle / 360 ) * 360
            if(45 <= angle && angle < 135) East
            else if(135 <= angle && angle < 225) South
            else if(225 <= angle && angle < 315) West
            else North
        }
        def distance(xn:Int=x0, yn:Int=y0) = {
            val dx = x - xn
            val dy = x - yn
            Math.sqrt(dx*dx + dy*dy)
        }
        val startdist = distance()
        //prioritize turns that point you in the correct direction
        //prioritize movement based on distance to the flag
        var collection:List[(Array[Card],Int)] = List()
        for(c<-hand.combinations(5)){
            for(p<-c.permutations){
                val testgame = game.copy
                for(card<-p){
                    testgame.playCard(robotNumber, card)
                    testgame.endRegisterPhase
                }
                //find ending position and facing
                val (xe, ye) = position(testgame)
                val fe = facing(testgame)
                val enddist = distance(xe, ye)
                val endchar = testgame.board(xe)(ye)
                //do tests to determine weight
                var weight = 0
                if(findangle(xe, ye)==fe) weight += 2
                if(enddist <= startdist) weight += (startdist-enddist).toInt max 1
                if( testgame.robots(robotNumber-1).getFlag > lookingfor )
                    weight = Int.MaxValue
                collection = (p, weight) +: collection
            }
        }
        var choice:Array[Card] = collection.maxBy(_._2)._1
        for(c<-hand) if(!choice.contains(c)) choice = choice :+ c
        choice
    }
}

class Greedy_George extends Personality {
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        def facing(t:Game=game) = t.robots(robotNumber-1).direction
        def position(t:Game=game):(Int, Int) = (t.robots(robotNumber-1).x, t.robots(robotNumber-1).y)
        //decide what flag you're on
        var lookingfor = game.robots(robotNumber-1).getFlag
        var (x, y) = game.flags(lookingfor)
        // println(robotNumber.toString+" is looking for flag "+lookingfor.toString)
        //find starting position
        var (x0:Int, y0:Int) = position()
        //find the general direction of the flag
        def findangle(xn:Int=x0, yn:Int=y0) = {
            var angle = Math.toDegrees(Math.atan2(xn - x, yn - y));
            angle = angle + Math.ceil( -angle / 360 ) * 360
            if(45 <= angle && angle < 135) East
            else if(135 <= angle && angle < 225) South
            else if(225 <= angle && angle < 315) West
            else North
        }
        def distance(xn:Int=x0, yn:Int=y0) = {
            val dx = x - xn
            val dy = x - yn
            Math.sqrt(dx*dx + dy*dy)
        }
        var localhand = hand.clone
        val chosen:Array[Card] = Array.fill(5)(null)
        var n = 3
        def getFirstMovement:Array[Card] = {
            var collection:List[(Array[Card],Int)] = List()
            val startdist = distance()
            for(c <- localhand.combinations(n)){
                for(p <- c.permutations){
                    //play out p
                    val testgame = game.copy
                    for(card<-chosen){
                        if(card != null){
                            testgame.playCard(robotNumber, card)
                            testgame.endRegisterPhase
                        }
                    }
                    for(card<-p){
                        testgame.playCard(robotNumber, card)
                        testgame.endRegisterPhase
                    }
                    //find ending position and facing
                    lookingfor = game.robots(robotNumber-1).getFlag
                    if(lookingfor < 3) {
                        val t = game.flags(lookingfor)
                        x = t._1
                        y = t._2
                    }
                    val (xe, ye) = position(testgame)
                    val fe = facing(testgame)
                    val enddist = distance(xe, ye)
                    val endchar = testgame.board(xe)(ye)
                    //do tests to determine weight
                    var weight = 0
                    if(findangle(xe, ye)==fe) weight += 2
                    if(enddist <= startdist) weight += (startdist-enddist).toInt max 1
                    if( testgame.robots(robotNumber-1).getFlag > lookingfor )
                        weight = Int.MaxValue
                    collection = (p, weight) +: collection
                }
            }
            collection.maxBy(_._2)._1
        }
        var n2 = 0
        while(n2 < 5 && n < 5){
            val t = getFirstMovement
            for(c<-t){
                chosen(n2) = c
                localhand = (localhand.toSet - c).toArray
                n2 += 1 
            }
            n = 5-n2
        }
        chosen ++ localhand
    }
}

class Silly_Sally extends Personality {
    def placeCards(robotNumber:Int,hand:Array[Card],game:Game):Array[Card] = {
        def facing(t:Game=game) = t.robots(robotNumber-1).direction
        def position(t:Game=game):(Int, Int) = (t.robots(robotNumber-1).x, t.robots(robotNumber-1).y)
        //decide what flag you're on
        var lookingfor = game.robots(robotNumber-1).getFlag
        var (x, y) = game.flags(lookingfor)
        // println(robotNumber.toString+" is looking for flag "+lookingfor.toString)
        //find starting position
        var (x0:Int, y0:Int) = position()
        //find the general direction of the flag
        def findangle(xn:Int=x0, yn:Int=y0) = {
            var angle = Math.toDegrees(Math.atan2(xn - x, yn - y));
            angle = angle + Math.ceil( -angle / 360 ) * 360
            if(45 <= angle && angle < 135) East
            else if(135 <= angle && angle < 225) South
            else if(225 <= angle && angle < 315) West
            else North
        }
        def distance(xn:Int=x0, yn:Int=y0) = {
            val dx = x - xn
            val dy = x - yn
            Math.sqrt(dx*dx + dy*dy)
        }
        var localhand = hand.clone
        var chosen:List[Card] = List()
        var n = 1
        def getFirstMovement:Array[Card] = {
            var collection:List[(Array[Card],Int)] = List()
            val startdist = distance()
            for(c <- localhand.combinations(n min localhand.size)){
                for(p <- c.permutations){
                    //play out p
                    val testgame = game.copy
                    for(card<-chosen){
                        if(card != null){
                            testgame.playCard(robotNumber, card)
                            testgame.endRegisterPhase
                        }
                    }
                    for(card<-p){
                        testgame.playCard(robotNumber, card)
                        testgame.endRegisterPhase
                    }
                    //find ending position and facing
                    lookingfor = game.robots(robotNumber-1).getFlag
                    if(lookingfor < 3) {
                        val t = game.flags(lookingfor)
                        x = t._1
                        y = t._2
                    }
                    val (xe, ye) = position(testgame)
                    val fe = facing(testgame)
                    val enddist = distance(xe, ye)
                    val endchar = testgame.board(xe)(ye)
                    //do tests to determine weight
                    var weight = 0
                    if(findangle(xe, ye)==fe) weight += 1
                    if(enddist <= startdist) weight += (startdist-enddist).toInt max 1
                    if( testgame.robots(robotNumber-1).getFlag > lookingfor )
                        weight = Int.MaxValue
                    collection = (p, weight) +: collection
                }
            }
            if(collection.isEmpty) return Array()
            val t = collection.maxBy(_._2)
            if(t._2 > 0) t._1
            else Array()
        }
        var n2 = 0
        while(n2 < 5 && n < 5){
            val t = getFirstMovement
            if(t.isEmpty) n += 1
            for(c<-t){
                if(n2 < 5){
                    chosen :+= c
                    localhand = (localhand.toSet - c).toArray
                    n2 += 1 
                }
            }
        }
        //println(chosen.map(_.attribute.toString).mkString(", "))
        //println(localhand.map(_.attribute.toString).mkString(", "))
        chosen.toArray ++ localhand
    }
}