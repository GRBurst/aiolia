package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.util.Vec2
import aiolia.util.DoubleBuffering

import collection.mutable

class Field(val dimensions: Vec2) {
  private val field: mutable.Map[Vec2, Thing] = mutable.HashMap.empty[Vec2, Thing]

  def creatures = field.collect{ case (pos, c: Creature) => pos -> c }
  def foods = field.collect{ case (pos, f: Food) => pos -> f }

  def lookup(pos: Vec2): Option[Thing] = field.get(pos)
  def lookupOption(pos: Vec2): Option[Option[Thing]] = if (isInside(pos)) Some(field.get(pos)) else None
  def apply(pos: Vec2): Option[Thing] = lookup(pos)
  def update(pos: Vec2, newValue: Thing) { update(pos, Some(newValue)) }
  def update(pos: Vec2, newValue: Option[Thing]) {
    newValue match {
      case Some(thing) =>
        field(pos) = thing
      case None =>
        field -= pos
    }
  }

  def clamp(pos: Vec2): Vec2 = Vec2(0 max pos.x min (dimensions.x - 1), 0 max pos.y min (dimensions.y - 1))
  def isInside(pos: Vec2): Boolean = 0 <= pos.x && pos.x < dimensions.x && 0 <= pos.y && pos.y < dimensions.y

  //TODO: move this val to Vec2
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

  def sensors(pos: Vec2, direction: Vec2, count: Int): Array[Double] = {
    val rotation = direction.angle
    val radius = 5 //TODO -> config
    val a = 2 * Math.PI / count
    (Array.tabulate(count){ i =>
      val startAngle = rotation + i * a
      val endAngle = rotation + (i + 1) * a
      val cone = neighbourCone(pos, radius, startAngle, endAngle).filter(isInside)
      val visible = cone.map(p => ((p distance pos) -> lookup(p))).collect{ case (d, Some(a)) => (d, a) }
      val visibleCreatures = visible.collect{ case (d, c: Creature) => (d, c) }
      val visibleFood = visible.collect{ case (d, f: Food) => (d, f) }
      Array(
        if (visibleCreatures.isEmpty) 0.0 else visibleCreatures.minBy(_._1)._2.appearance,
        if (visibleFood.isEmpty) 0.0 else visibleFood.minBy(_._1)._2.appearance
      )
    }).flatten
  }

  override def toString = (Vec2(0, 0) untilByY dimensions).map(p => field.get(p)).grouped(dimensions.x).map{ line =>
    def desc(place: Option[Thing]) = place match {
      case Some(c: Creature) =>
        // val energy = s"${(c.energy * 10).toInt.toHexString}"
        val energy = ("%6.4f" format c.energy).drop(2)
        val hash = c.##.toString.take(4)
        val horniness = s"${((c.brain.horniness + 1) * 5).toInt.toHexString}"
        val wantsToReplicate = c.brain.horniness > 0
        val wantsToFight = c.brain.agression > 0
        val want = if (wantsToReplicate && wantsToFight) "*" else { if (wantsToReplicate) "+" else { if (wantsToFight) "x" else " " } }
        val age = s"${c.age}"
        val logAge = Math.log(c.age + 1).floor.toInt.toString.take(1).mkString
        s"${logAge}${energy.take(1)}$want"
      case Some(f: Food) => s" ${f.symbol}"
      case None          => ""
    }
    val w = 3
    line.map(desc(_).padTo(w, " ").mkString).mkString // + "\n" + "-" * (w + 1) * line.size

  }.mkString("\n")

}

//TODO: don't use double buffering. nextGen should always start with a fresh hashmap
class World(val dimensions: Vec2) extends DoubleBuffering[Field] {
  //TODO: idea: HashSet[Vec2, Thing] for unlimited sized worlds
  def bufferInit = new Field(dimensions)
  def gen = buffer
  def nextGen = nextBuffer

  override def toString = buffer.toString
}
