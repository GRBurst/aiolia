package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.util.Vec2

import collection.mutable

class World(val dimensions: Vec2) {
  //TODO: idea: HashSet[Vec2, Thing] for unlimited sized worlds
  private val field: Array[Array[Option[Thing]]] = Array.fill(dimensions.y, dimensions.x)(None)

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
    assert(creatures.size == field.flatten.flatten.collect{ case c: Creature => c }.size)
  }
  def remove(thing: Thing) {
    assert(things contains thing)
    assert(lookup(thing.pos) == Some(thing))
    update(thing.pos, None)
    _things -= thing
    assert(creatures.size == field.flatten.flatten.collect{ case c: Creature => c }.size)
  }
  def move(thing: Thing, newPos: Vec2) {
    assert(things contains thing)
    assert(lookup(thing.pos) == Some(thing))
    val oldPos = thing.pos
    lookup(newPos) match {
      case Some(thing) =>
        throw new IllegalStateException(s"Trying to move $thing to occupied position: ${newPos}")
      case None =>
        update(oldPos, None)
        update(newPos, Some(thing))
        thing.pos = newPos
    }
    assert(creatures.size == field.flatten.flatten.collect{ case c: Creature => c }.size)
  }

  def creatures = things.collect{ case c: Creature => c }
  def foods = things.collect{ case c: Food => c }

  def lookup(pos: Vec2): Option[Thing] = field(pos.y)(pos.x)
  def lookupOption(pos: Vec2): Option[Option[Thing]] = if (isInside(pos)) Some(field(pos.y)(pos.x)) else None
  def apply(pos: Vec2): Option[Thing] = lookup(pos)
  private def update(pos: Vec2, newValue: Option[Thing]) { field(pos.y)(pos.x) = newValue }
  def clamp(pos: Vec2): Vec2 = Vec2(0 max pos.x min (dimensions.x - 1), 0 max pos.y min (dimensions.y - 1))
  def isInside(pos: Vec2): Boolean = 0 <= pos.x && pos.x < dimensions.x && 0 <= pos.y && pos.y < dimensions.y

  val directions = Array(Vec2(0, 1), Vec2(1, 0), Vec2(1, 1), Vec2(0, -1), Vec2(-1, 0), Vec2(1, -1), Vec2(-1, 1), Vec2(-1, -1))
  def neigbourPositions(pos: Vec2): Set[Vec2] = directions.map(pos + _).filter(isInside).toSet
  def neighbours(pos: Vec2): Set[Thing] = neigbourPositions(pos).flatMap(lookup)
  def emptyNeighbourPositions(pos: Vec2): Set[Vec2] = neigbourPositions(pos).filter(lookup(_).isEmpty)
  def neighbourCirclePositions(pos: Vec2, radius: Double) = { // without pos
    val radiusSq = radius * radius
    for (
      x <- (pos.x - radius.ceil.toInt) to (pos.x + radius.ceil.toInt);
      y <- (pos.y - radius.ceil.toInt) to (pos.y + radius.ceil.toInt);
      if pos != Vec2(x, y) && ((pos.x - x) * (pos.x - x) + (pos.y - y) * (pos.y - y)) < radiusSq
    ) yield Vec2(x, y)
  }
  def neighbourCone(pos: Vec2, radius: Double, startAngle: Double, endAngle: Double) = neighbourCirclePositions(pos, radius).filter{ p =>
    val a = (p - pos).angle
    if (startAngle < endAngle) startAngle <= a && a <= endAngle
    else {
      val d = 2 * Math.PI - startAngle
      0 <= (a + d) && a <= endAngle
    }
  }

  def appearanceAt(pos: Vec2): Double = lookupOption(pos) match {
    case None => 0.1 // outside field / wall
    case Some(thingOption) => thingOption match {
      case None              => 0.0 // empty position
      case Some(_: Food)     => 1.0
      case Some(c: Creature) => -c.brain.agression
    }
  }

  def sensors(pos: Vec2, rotation: Double, count: Int): Array[Double] = {
    val radius = 10 //TODO -> config
    val a = 2 * Math.PI / count
    (Array.tabulate(count){ i =>
      val startAngle = rotation + i * a
      val endAngle = rotation + (i + 1) * a
      val cone = neighbourCone(pos, radius, startAngle, endAngle)
      val visible = cone.map(p => ((p distance pos) -> appearanceAt(p))).filter{ case (_, a) => a != 0.0 }
      if (visible.isEmpty) Array(1.0, 0.0)
      else {
        val (distance, appearance) = visible.minBy{ case (distance, _) => distance }
        Array(distance, appearance)
      }
    }).flatten
  }

  override def toString = field.map { line =>
    def desc(place: Option[Thing]) = place match {
      case Some(c: Creature) =>
        val energy = s"${(c.energy * 10).toInt.toHexString}"
        val horniness = s"${((c.brain.horniness + 1) * 5).toInt.toHexString}"
        val canReplicate = s"${if (c.canReplicate) "+" else " "}"
        val age = s"${c.age}"
        s"$age"
      case Some(f: Food) => s" ${f.symbol}"
      case None          => ""
    }
    line.map(desc(_).padTo(4, " ").mkString).mkString("|") + "\n" + "-" * 5 * line.size
  }.mkString("\n")
}
