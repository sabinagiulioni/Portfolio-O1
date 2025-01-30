package o1.adventure

import scala.collection.mutable.Map
import o1.*

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Returns the player’s current location. */
  def location = this.currentLocation

  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    if destination.isEmpty then
      "You can't enter the " + direction + "." //the player can't go directly to that space because there is no door
    else if this.location.items.nonEmpty then
      "Before moving, you need to collect the object." //the player can go to that space, but only after collecting the object from the current location
    else
      destination match
        case Some(room) =>
          if room.locked then //the only locked room is the balcony
            "The door to the balcony is locked. Use the key to open it."
          else if currentLocation.lights then //you can leave a room only if its lights are on
            currentLocation = room
            "You enter the " + direction + "."
          else //if the ghost turns off the lights, you need to turn the flashlight on before entering the next room
            "Oh no! The ghost turned off the lights, you can't see anything!\nNow you can't enter the " + direction + ". Use your flashlight to continue your adventure."
        case None => ""


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You can rest for a bit, but not for long.\nI bet you don't want to spend the whole day looking for a key..."


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "You're now in the " + this.location.name

  private val inventoryVector = Map[String, Item]()

  def examine(itemName: String): String = //you can examine an object in its area both before and after picking it up. You can examine objects that you already picked up in every area.
    if this.location.contains(itemName) then
      val areaItem = Option(this.location.items(itemName))
      areaItem match
       case Some(item) => item.description
       case None => ""
    else if this.inventoryVector.contains(itemName) then
      inventoryVector(itemName).description
    else
      f"There's no $itemName here."

  def get(itemName: String): String =
    this.location.removeItem(itemName) match
      case Some(item) => inventoryVector += itemName -> item
        val itemDescription = inventoryVector(itemName)
        if itemName == "key" then
          f"You got it! Go to the balcony and use the key."
        else if itemName == "glass of water" then
          "You got a glass of water. You can drink it at any point during the game, but only once."
        else
          f"You pick up the $itemName."
      case None => f"There is no $itemName here to pick up."

  def has(itemName: String): Boolean =
    this.inventoryVector.contains(itemName)

  def inventory: String =
    if inventoryVector.isEmpty then
      "You are empty-handed."
    else
       s"You are carrying:\n${this.inventoryVector.keys.mkString("\n")}"

  def lights(): String= //turns on the lights
    this.location.switchLights()
    "Now you can see, let's go!"

  private val valzer = "ceggcehfff-hfggfgceee-ceggcehfff-hfggfgcegc-/60"

  def use(itemName: String): String=
    if this.inventoryVector.contains(itemName) then
      if itemName == "hand lens" && this.location.name == "Library" then
        "Damn, look at all that dust! They should wipe those books..."
      else if itemName == "pair of dancing shoes" && this.location.name == "Ballroom" then
        play(valzer)
        "Can you hear this melody? Time to show off your moves!\n\nAfter this dance break, you can go on exploring."
      else if itemName == "glass of water" then //drink the glass of water = use the glass of water, at any point during the game
        this.inventoryVector -= "glass of water"
        "Aaaah, that was refereshing."
      else if itemName == "key" then //if you have collected the key,
        if this.location.name != "Tower" then //but are not in the tower:
          "Use the key in the right room."
        else
          val balcony = this.location.neighbor("balcony") //if you are in the tower you unlock the door
          balcony match
            case Some(bal) => bal.locked = false
          "You unlocked the door."
      else if itemName == "flashlight" && this.location.name == "Library" || this.location.name == "Bedroom" then
        this.lights()
      else //you try to use an object where you don't need it
      "There's no use for that object here."
    else
      "You don't have that object."


  def help(): String=
    "\nYou are asked to collect objects and use them when you want/need them. You can also examine the objects you have collected.\nYou cannot exit a room before collecting the object stored there.\nFirst get the object, then move.\n\nYou can use every object - some you must use to go on with the game, others are not that useful. \n\nInstructions for playing:\n- to move to a space enter 'go x'\n- to examine an object enter 'examine x'\n- to pick up an object enter 'get x'\n- to use an object enter 'use x'\n- to drink the glass of water enter 'drink'\n- to take a break enter 'rest'\n- to see what objects you have enter 'inventory'\n- to quit the game enter 'quit'\n'x' is either the name of a space or an object.\n\nNames of the objects:\n- flashlight\n- hand lens\n- pair of dancing shoes\n- glass of water\n- key\nType them exactly like they are written here."

  
  def drink()=
    if this.inventoryVector.contains("glass of water") then
      this.use("glass of water")
    else
      "You don't have any water to drink."

end Player