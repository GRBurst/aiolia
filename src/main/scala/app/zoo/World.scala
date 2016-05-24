package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.util.Vec2

import collection.mutable

class World(val dimensions: Vec2) {
  //TODO: idea: HashSet[Vec2, Thing] for unlimited sized worlds
  private val field: Array[Array[Option[Thing]]] = Array.tabulate(dimensions.x, dimensions.y)((x, y) => None)

  private val _things: mutable.Set[Thing] = mutable.HashSet.empty[Thing]
  def things: collection.Set[Thing] = _things
  //TODO: performance: manage additional lists for creature and food?
  def add(thing: Thing) {
    if (lookup(thing.pos).isEmpty) {
      _things += thing
      update(thing.pos, Some(thing))
    }
    else
      throw new IllegalStateException(s"Trying to add $thing on occupied position: ${thing.pos}")
  }
  def remove(thing: Thing) {
    _things -= thing
    update(thing.pos, None)
  }
  def move(thing: Thing, newPos: Vec2) {
    val oldPos = thing.pos
    lookup(newPos) match {
      case Some(thing) =>
        throw new IllegalStateException(s"Trying to move $thing to occupied position: ${newPos}")
      case None =>
        update(oldPos, None)
        update(newPos, Some(thing))
        thing.pos = newPos
    }
  }

  def creatures = things.collect{ case c: Creature => c }
  def foods = things.collect{ case c: Food => c }

  def lookup(pos: Vec2): Option[Thing] = field(pos.x)(pos.y)
  def lookupOption(pos: Vec2): Option[Option[Thing]] = if (isInside(pos)) Some(field(pos.x)(pos.y)) else None
  def apply(pos: Vec2): Option[Thing] = lookup(pos)
  private def update(pos: Vec2, newValue: Option[Thing]) { field(pos.x)(pos.y) = newValue }
  def clamp(pos: Vec2): Vec2 = Vec2(0 max pos.x min (dimensions.x - 1), 0 max pos.y min (dimensions.y - 1))
  def isInside(pos: Vec2): Boolean = 0 <= pos.x && pos.x < dimensions.x && 0 <= pos.y && pos.y < dimensions.y

  val directions = Array(Vec2(0, 1), Vec2(1, 0), Vec2(1, 1), Vec2(0, -1), Vec2(-1, 0), Vec2(1, -1), Vec2(-1, 1), Vec2(-1, -1))
  def neigbourPositions(pos: Vec2): Set[Vec2] = directions.map(pos + _).filter(isInside).toSet
  def neighbours(pos: Vec2): Set[Thing] = neigbourPositions(pos).flatMap(lookup)
  def emptyNeighbourPositions(pos: Vec2): Set[Vec2] = neigbourPositions(pos).filter(lookup(_).isEmpty)

  def sensors(pos: Vec2, rotation: Double) = directions.map(pos + _.rotate(rotation)).map { pos =>
    lookupOption(pos) match {
      case None => -1.0 // outside field / wall
      case Some(thingOption) => thingOption match {
        case None              => 0.0 // empty position
        case Some(_: Food)     => 0.5
        case Some(_: Creature) => 0.5
      }
    }
  }

  override def toString = field.map { line =>
    def desc(place: Option[Thing]) = place match {
      case Some(c: Creature) => s"${(c.energy * 10).toInt.toHexString},${"%+f".format(c.replicationStrength).head}${(c.replicationStrength.abs * 10).toInt.toHexString}"
      case Some(f: Food)     => s" F"
      case None              => ""
    }
    line.map(desc(_).padTo(4, " ").mkString).mkString("|") + "\n" + "-" * 5 * line.size
  }.mkString("\n")
}
