package o1.adventure

import scala.io.StdIn.readLine
import scala.util.Random
import o1.*

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of “hard-coded” information that pertains to a very
  * specific adventure game that involves a quest in a castle. All newly created
  * instances of class `Adventure` are identical to each other. */
class Adventure:

  /** the name of the game */
  val title = "A Ghostly Trick"

  private val start       = Area("Courtyard", "You are in the courtyard of the castle.\nThere are four doors leading to different rooms.", true, false)
  private val library     = Area("Library", "You entered one of the most magical rooms of the castle, and now you are surrounded by centuries-old books and documents.", false, false)
  private val bedroom     = Area("Bedroom", "This is the bedroom of the count - don't tell him you've been here.", false, false)
  private val ballroom    = Area("Ballroom", "Welcome to the ballroom! Take a moment to admire it: look at the sculptures, the pavement, the frescos...\nThe price for the frescoed ceiling was very high, though.", true, false)
  private val tower       = Area("Tower", "You are in the highest tower of the castle, finally. Are you tired after all those stairs?", true, false)
  private val balcony     = Area("Balcony", "From here you can see the neighboring villages, far to the Po valley, and the Monferrato hills.", true, true)
  private val destination = balcony

  start   .setNeighbors(Vector("library" -> library, "bedroom" -> bedroom, "ballroom" -> ballroom, "tower" -> tower                                              ))
  library .setNeighbors(Vector(                      "bedroom" -> bedroom,                         "tower" -> tower, "courtyard" -> start                        ))
  bedroom .setNeighbors(Vector("library" -> library,                       "ballroom" -> ballroom,                   "courtyard" -> start                        ))
  ballroom.setNeighbors(Vector(                      "bedroom" -> bedroom,                                           "courtyard" -> start                        ))
  tower   .setNeighbors(Vector("library" -> library,                                                                 "courtyard" -> start,   "balcony" -> balcony))
  balcony .setNeighbors(Vector(                                                                     "tower" -> tower                                             ))

  private val key = Item("key", "The shiny, metal key.")
  private val lens = Item("hand lens", "An old hand lens. Undoubtedly useful, but it doesn't open a door...")
  private val flashlight = Item("flashlight", "A flashlight. Might come in handy during this quest, take it with you!")
  private val water = Item("glass of water", "A glass of water! If the ghost put it here, it was nice of him.\nDrink some water after all those steps, take the glass.")
  private val shoes = Item("pair of dancing shoes", "Seek and ye shall find, right? You never know when you might end up on the dance floor. Try them on (use them)!")

  this.bedroom.addItem(key)
  this.library.addItem(lens)
  this.start.addItem(flashlight)
  this.tower.addItem(water)
  this.ballroom.addItem(shoes)
 
  /** The character that the player controls in the game. */
  val player = Player(start)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 40

  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = 
    this.player.location == this.balcony && this.player.has("key")

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage =
    f"Welcome to the Sannazzaro Castle, $name! The Sannazzaro family has owned this castle for over 900 years, since their forefathers built it in Giarole, a small village in northern Italy.\nThe castle was renovated in the nineteenth century, and for that occasion the count ordered frescoes to the ceiling of the ballroom. Unfortunately, while painting them, painter Grosso fell from a ladder and died... To this day, his ghost is still haunting the castle and tricking its guests.\n\nYou want to enjoy the view from the balcony of the tower, but the ghost hid the key!\nFind the key, head to the balcony and enjoy your stay at the castle.\n\nThere are interesting objects everywhere - examine, collect, and use them to move to new spaces."

 //The players are asked for their name at the beginning of each game
  val name = readLine("Type your name: ")

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether the player has completed their quest. */
  def goodbyeMessage =
    if this.isComplete then
      f"Finally! You made it to the balcony. ${balcony.description}\nEnjoy the scenery, and prepare for other ghostly tricks during your stay at the Sannazzaro Castle!"
    else if this.turnCount == this.timeLimit then
      "Oh no! Time's up, the carriage is ready to take you on a tour of the village.\nTry again later - game over for now!"
    else  // game over due to player quitting
      "I guess the ghost won this time..."

  
  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  // In addition to what is explained above, the method prints the message of the ghost given by moveGhost() when a 'go' command is given
  def playTurn(command: String): String =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    if outcomeReport.isDefined then
      this.turnCount += 1
      if command.contains("go") then
        outcomeReport.getOrElse(s"""Unknown command: "$command".""") + f"\n${moveGhost()}"
      else
        outcomeReport.getOrElse(s"""Unknown command: "$command".""")
    else
      s"""Unknown command: "$command"."""

  private val generator = Random(1)
  private def pickRandomArea(): Area = //this method randomly returns an area in which the ghost is at that specific turn (the ghost has only three alternatives)
    val directionVec = Vector[Area](start, ballroom, tower)
    val randomIndex = generator.nextInt(3)
    directionVec(randomIndex)

  private val spooky = "d-h-f---"
  def moveGhost()= //moves the ghost to the random location given by the pickRandomArea() method. If the player is in the same area, the ghost's message is printed out to frighten the player and a spooky melody is played
    val ghostLoc = pickRandomArea()
    if ghostLoc == this.player.location then
      play(spooky)
      f"\nBooo! I'm the ghost, and I'm right behind you - did I scare you, $name?\nDon't worry about me, go on with the game."
    else
      ""

end Adventure

