package o1.adventure
import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. Here, an "area" is a room of the castle. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String, var lights: Boolean, var locked: Boolean):

  private val neighbors = Map[String, Area]()

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction–area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) =
    this.neighbors ++= exits


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. If there are no
    * items present, the return value has the form "DESCRIPTION\n\nExits available:
    * DIRECTIONS SEPARATED BY SPACES". If there are one or more items present, the return
    * value has the form "DESCRIPTION\nYou see here: ITEMS SEPARATED BY SPACES\n\nExits available:
    * DIRECTIONS SEPARATED BY SPACES". The items and directions are listed in an arbitrary order. */
  def fullDescription: String =
    val exitList = "\n\nWhere do you want to go? These are your alternatives: " + this.neighbors.keys.mkString(" ")
    if this.items.isEmpty then
      this.description + exitList
    else
      val itemAvailable = "\n\nAfter searching thoroughly, here you found a " + this.items.keys.head + ".\nExamine it and take it with you!"
      this.description + itemAvailable


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString =
    this.name + ": " + this.description.replaceAll("\n", " ").take(150)

  val items = Map[String, Item]()
  def addItem(item: Item) =
    this.items += item.name -> item

  def contains(itemName: String): Boolean =
    this.items.keys.toVector.contains(itemName)

  def removeItem(itemName: String): Option[Item] =
      val itemToRemove = this.items.get(itemName)
      itemToRemove match
        case Some(item) => this.items -= itemName
        case None => 
      itemToRemove

  def switchLights()= //this method turns on the lights in a room
    this.lights = true

end Area

