package proj

import org.scalatest.FunSpec
import org.scalatest.Matchers
import proj._

class GameTest extends FunSpec with Matchers {
    val jimsCard = new Card(Move1, 1)
    describe("A Card"){
        it("should have a card type"){
            assert(jimsCard.attribute==Move1)
        }
        it("should have a priority number"){
            assert(jimsCard.priority == 1)
        }
    }
    val jimsHand = Array.fill(7)( jimsCard )
    val jim = new Robot(0,0,1,North, jimsHand)
    describe("A Robot"){
        it("should hold a position") {
            assert(jim.x == 0)
            assert(jim.y == 0)
        }
        it("should have a number") {
            assert(jim.robotNumber==1)
        }
        it("should face a direction"){
            assert(jim.direction==North)
        }
        it("should have a hand of cards"){
            assert(jim.hand == jimsHand)
        }
        it("should keep track of how many flags it has found"){
            assert(jim.getFlag == 0)
        }
    }
    
    describe("A Deck"){
        def getCards(deck:Deck) = (for(i<-1 to 84) yield deck.draw)
        describe("when initialized have"){
            def ofType(ctype:CardType,deck:Deck):Int = {
                val allTypes = getCards(deck).map(_.attribute)
                allTypes.foldRight(0){(c,a) => 
                    if(c==ctype)a+1 else a }
            }
            it("84 total cards"){
                val deck = new Deck
                var allCards = getCards(deck)
                intercept[NoSuchElementException]
                {deck.draw}
                assert(allCards.length == 84)
            }
            it("18 move1 cards"){
                val deck = new Deck
                assert(ofType(Move1, deck) == 18)
            }
            it("12 move2 cards"){
                val deck = new Deck
                assert(ofType(Move2, deck) == 12)
            }
            it("6 move3 cards"){
                val deck = new Deck
                assert(ofType(Move3, deck) == 6)
            }
            it("6 backup cards"){
                val deck = new Deck
                assert(ofType(BackUp, deck) == 6)
            }
            it("18 RotateRight cards"){
                val deck = new Deck
                assert(ofType(RotateRight, deck) == 18)
            }
            it("18 RotateLeft cards"){
                val deck = new Deck
                assert(ofType(RotateLeft, deck) == 18)
            }
            it("6 uturn cards"){
                val deck = new Deck
                assert(ofType(UTurn, deck) == 6)
            }
        }
        it("should be able to shuffle"){
            val deck = new Deck
            val top = deck.draw
            deck.replace(top)
            deck.shuffle
            val newTop = deck.draw
            assert(top != newTop)
        }
        it("should enforce unique priority numbers amung its cards"){
            val deck = new Deck
            val priorities = getCards(deck).map(_.priority)
            assert(priorities.toSet.size == priorities.length)
        }
        it("should be able to remove drawn cards"){
            val deck = new Deck
            val top = deck.draw
            val remainder = (for(i<-1 to 83) yield deck.draw).toSet
            assert(!remainder(top))
        }
        it("should be able to replace cards"){
            val deck = new Deck
            val top = deck.draw
            deck.replace(top)
            val cards = getCards(deck).toSet
            assert(cards(top))
        }
    }
    describe("A Register"){
        val r = new Register
        val cards = jimsHand.dropRight(2)
        it("should update the register when given 5 new cards"){
            r.updateRegister(cards)
            assert(r.viewRegister == cards)
        }
        it("should show cards in the register"){
            r.updateRegister(cards)
            assert(r.viewRegister == cards)
        }
        it("should hold 5 cards"){
            r.updateRegister(cards)
            assert(r.viewRegister.length == 5)
        }
    }
    describe("A Game"){
        it("has a game board"){
            val game = new Game 
            game.init
            assert(game.getLayout!=null)
        }
        it("has 4 robots"){
            val game = new Game 
            game.init
            assert(game.getRobotLocations.length == 4)
        }
        it("has as many registers as robots"){
            val game = new Game 
            game.init
            assert(game.viewRegisters.size == game.getRobotLocations.length)
        }
        it("has as many starting blocks as robots"){
            val game = new Game 
            game.init
            assert(game.startBlocks.length == game.getRobotLocations.length)
        }
        describe("when initialized"){
            it("should be able to read the board"){
                val game = new Game 
                game.init
                assert(game.interpretBoard != null)
            }
        }
        it("should be able to return the board layout"){
            val game = new Game 
            game.init
            //nothing has changed in the game then the layout should equal the init state
            assert(game.interpretBoard.deep == game.getLayout.deep)
        }
        it("should be able to return robot locations"){
            val game = new Game 
            game.init
            assert(game.getRobotLocations.deep == game.startBlocks.toArray.deep)
        }
        it("should be able to enact a player's card"){
            val game = new Game 
            game.init
            val startPos = game.getRobotLocations(0)
            game.playCard(1,new Card(Move1,0))
            val endPos = game.getRobotLocations(0)
            assert(startPos._1==endPos._1 && startPos._2-1 == endPos._2)
        }
        it("should return a player to the starting block if they fall in a hole or go off the edge"){
            val game = new Game 
            game.init
            val startPos = game.startBlocks(0)
            game.playCard(1, new Card(UTurn,0))
            game.playCard(1, new Card(Move1,0))
            val endPos = game.getRobotLocations(0)
            assert(startPos == endPos)
        }
        it("should show player registers"){
            val game = new Game 
            game.init
            val jimsRegister = new Register
            jimsRegister.updateRegister(jimsHand.dropRight(2))
            game.updateRegister(1,jimsRegister,jimsHand.drop(5))
            assert(game.viewRegisters(0).viewRegister == jimsRegister.viewRegister)
        }
        it("should show the score"){
            val game = new Game 
            game.init
            assert(game.getScore.deep == Array.fill(4)(0).deep )
        }
        it("should be able to determine the player order given a register phase"){
            val c = (1 to 4).toArray.map{ i=>
                val r = new Register
                r.updateRegister(Array.fill(5)(new Card(Move1, 4-i)))
                r
            }
            val game = new Game 
            game.init
            for(i<-c.indices) game.updateRegister(i+1, c(i), null)
            assert(game.getOrder(0) == List(4,3,2,1))
        }
        describe("at the end of a register phase"){
            it("should move all the conveyer belts"){
                val game = new Game 
                game.init
                game.playCard(2,new Card(Move3,0))
                game.playCard(2,new Card(Move2,0))
                val startPos = game.getRobotLocations(1)
                game.endRegisterPhase
                val endPos = game.getRobotLocations(1)
                assert(startPos._1==endPos._1 && startPos._2-1 == endPos._2)
            }
        }
        describe("at the end of a turn"){
            it("should replace all cards in the registers"){
                val game = new Game 
                game.init
                for (i<- 1 to 4){
                    val r = new Register
                    r.updateRegister(game.showHand(i).dropRight(2))
                    game.updateRegister(i,r,game.showHand(i).drop(5))
                }
                val startlen = game.decksize
                game.replaceCards // not game.endTurn but there is no way to pause and know
                val endlen = game.decksize
                assert(startlen + 5*4 == endlen)
            }
            it("should shuffle the deck"){
                val game = new Game 
                game.init
                var cards:Set[Card] = Set()
                for (i<- 1 to 4){
                    val r = new Register
                    cards = game.showHand(i).dropRight(2).toSet[Card] union cards
                    r.updateRegister(game.showHand(i).dropRight(2))
                    game.updateRegister(i,r,game.showHand(i).drop(5))
                }
                game.endTurn
                var newCards:Set[Card] = Set()
                for(i<-1 to 4) newCards = game.showHand(i).dropRight(2).toSet union newCards
                assert(newCards != cards)
            }
            it("should give each player 5 new cards"){
                val game = new Game 
                game.init
                val hands:Array[Array[Card]] = Array.fill(4)(null)
                for (i<- 1 to 4){
                    val r = new Register
                    hands(i-1) = game.showHand(i)
                    r.updateRegister(game.showHand(i).dropRight(2))
                    game.updateRegister(i,r,game.showHand(i).drop(5))
                }
                game.endTurn
                var endhand:Array[Array[Card]] = Array.fill(4)(null)
                for (i<- 1 to 4){
                    endhand(i-1) = game.showHand(i)
                }
                assert(hands.deep != endhand.deep)
            }
        }
    }
    describe("AI Personalities") {
        // Greedy George
        // Bobby Fischer
        // Ray Charles
        // Lazy Leo
        val model = new Model
        val gameSim = new GameSim(model.game, model.po)

        it("can make a move") { noException should be thrownBy gameSim.doTurn }
        it("should be able to place cards") { noException should be thrownBy gameSim.doTurn }
        it("only plays cards from its hand") { noException should be thrownBy gameSim.doTurn }
        it("should be able to simultate future moves without changing game") { noException should be thrownBy gameSim.doTurn }
        it("should be able to win in 100 turns or less") { noException should be thrownBy gameSim.doGame }
    }
}