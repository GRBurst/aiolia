package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.util.Vec2
import aiolia.util.DoubleBuffering

import collection.mutable

class Field(val dimensions: Vec2) {

  val field: Array[Array[Option[Thing]]] = Array.fill(dimensions.y, dimensions.x)(None)

  private val _things: mutable.Set[Thing] = mutable.HashSet.empty[Thing]
  def things: collection.Set[Thing] = _things
  //TODO: performance: manage additional lists for creature and food?
  def move(thing: Thing, newPos: Vec2) {
    update(thing.pos, None)
    update(newPos, Some(thing))
  }

  def creatures = things.collect{ case c: Creature => c }
  def foods = things.collect{ case c: Food => c }

  def lookup(pos: Vec2): Option[Thing] = field(pos.y)(pos.x)
  def lookupOption(pos: Vec2): Option[Option[Thing]] = if (isInside(pos)) Some(field(pos.y)(pos.x)) else None
  def apply(pos: Vec2): Option[Thing] = lookup(pos)
  def update(pos: Vec2, newValue: Thing) { update(pos, Some(newValue)) }
  def update(pos: Vec2, newValue: Option[Thing]) {
    assert(creatures.size == field.flatten.flatten.collect{ case c: Creature => c }.size)
    (newValue, field(pos.y)(pos.x)) match {
      case (Some(thing), None) =>
        _things += thing; thing.pos = pos
      case (None, Some(thing)) =>
        assert(things contains thing)
        assert(lookup(thing.pos) == Some(thing))
        _things -= thing
      case (Some(thing1), Some(thing2)) =>
        if (thing1 != thing2)
          throw new IllegalStateException(s"Trying to add $thing1 on occupied position: ${thing1.pos}")
      case (None, None) =>
    }
    field(pos.y)(pos.x) = newValue
    assert(creatures.size == field.flatten.flatten.collect{ case c: Creature => c }.size)
  }
  def clamp(pos: Vec2): Vec2 = Vec2(0 max pos.x min (dimensions.x - 1), 0 max pos.y min (dimensions.y - 1))
  def isInside(pos: Vec2): Boolean = 0 <= pos.x && pos.x < dimensions.x && 0 <= pos.y && pos.y < dimensions.y

  val directions = Array(Vec2(0, 1), Vec2(1, 0), Vec2(1, 1), Vec2(0, -1), Vec2(-1, 0), Vec2(1, -1), Vec2(-1, 1), Vec2(-1, -1))
  def positions = Vec2(0, 0) until dimensions
  def neighbourPositions(pos: Vec2): Set[Vec2] = directions.map(pos + _).filter(isInside).toSet
  def neighbours(pos: Vec2): Set[Thing] = neighbourPositions(pos).flatMap(lookup)
  def emptyNeighbourPositions(pos: Vec2): Set[Vec2] = neighbourPositions(pos).filter(lookup(_).isEmpty)
  def neighbourCirclePositions(center: Vec2, radius: Double) = { // without center
    val radiusSq = radius * radius
    val intRadius = radius.ceil.toInt
    ((center - intRadius) to (center + intRadius)).filter(p => center != p && p.distanceSq(center) <= radiusSq)
  }

  def betweenAngle(start: Double, a: Double, end: Double): Boolean = {
    // TODO: test cases
    // assert(betweenAngle(0.1, 0.2, 0.3))
    // assert(!betweenAngle(-0.1, -0.2, -0.3))
    // assert(betweenAngle(-0.1, 1, -1))
    // assert(betweenAngle(-Math.PI / 2, 0.1, Math.PI / 2))
    // assert(betweenAngle(-Math.PI / 2, -0.1, Math.PI / 2))

    def modulo(a: Double, b: Double) = { val r = a % b; if (r < 0) r + b else r }
    def pa(a: Double) = modulo(a, 2 * Math.PI) // positive angle

    if (pa(start) <= pa(end))
      pa(start) <= pa(a) && pa(a) <= pa(end)
    else {
      val d = 2 * Math.PI - pa(start)
      0 <= pa(a + d) && pa(a + d) <= pa(end + d)
    }
  }

  def neighbourCone(center: Vec2, radius: Double, startAngle: Double, endAngle: Double) = {
    neighbourCirclePositions(center, radius).filter{ p =>
      betweenAngle(startAngle, (p - center).angle, endAngle)
    }
  }

  def appearanceAt(pos: Vec2): Double = lookupOption(pos) match {
    case None              => -0.1 // outside field / wall
    case Some(thingOption) => thingOption.map(_.appearance).getOrElse(0.0)
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

class World(val dimensions: Vec2) extends DoubleBuffering[Field] {
  //TODO: idea: HashSet[Vec2, Thing] for unlimited sized worlds
  def bufferInit = new Field(dimensions)
  def gen = buffer
  def nextGen = nextBuffer

  override def toString = buffer.toString
}
