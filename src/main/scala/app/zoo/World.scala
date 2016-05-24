package aiolia.app.zoo

import aiolia.grammar.Grammar
import aiolia.util.Vec2

import collection.mutable

class Place(val position: Vec2) {
  var creature: Option[Creature] = None
  val foods: mutable.Set[Food] = mutable.HashSet.empty

  def isEmpty = creature.isEmpty && foods.isEmpty

  override def toString = s"Place($position, $creature, $foods)"
}

class World(val dimensions: Vec2) {
  val field: Array[Array[Place]] = Array.tabulate(dimensions.x, dimensions.y)((x,y) => new Place(Vec2(x,y)))

  private val _creatures: mutable.Set[Creature] = mutable.HashSet.empty
  def creatures: Iterable[Creature] = _creatures

  def apply(position: Vec2): Place = field(position.x)(position.y)
  def clamp(position: Vec2): Vec2 = Vec2(0 max position.x min (dimensions.x - 1), 0 max position.y min (dimensions.y - 1))
  def isInside(position: Vec2): Boolean = 0 <= position.x && position.x < dimensions.x && 0 <= position.y && position.y < dimensions.y

  def neigbourPositions(position: Vec2): Set[Vec2] = directions.map(position + _).filter(isInside).toSet
  def neighbourPlaces(position: Vec2): Set[Place] = neigbourPositions(position).map(apply)

  def place(food: Food, position: Vec2) {
    apply(position).foods += food
  }

  def remove(food: Food, position: Vec2) {
    apply(position).foods -= food
  }

  def place(creature: Creature) {
    val place = apply(creature.position)
    if (place.creature.nonEmpty)
      throw new IllegalStateException(s"Trying to place creature on occupied place: $place")

    _creatures += creature
    place.creature = Some(creature)
  }

  def move(creature: Creature, position: Vec2) {
    val place = apply(position)
    if (place.creature.nonEmpty)
      throw new IllegalStateException(s"Trying to move creature on occupied place: $place")

    val oldPos = creature.position
    apply(oldPos).creature = None
    creature.position = position
    place.creature = Some(creature)
  }

  def remove(creature: Creature) {
    _creatures -= creature
    apply(creature.position).creature = None
  }

  val directions = Array(Vec2(0,1), Vec2(1,0), Vec2(1,1), Vec2(0,-1), Vec2(-1,0), Vec2(1,-1), Vec2(-1,1), Vec2(-1,-1))

  def sensors(position: Vec2) = directions.map(position + _).map { pos =>
    if (isInside(pos)) {
      val place = apply(pos)
      val creatureScore = place.creature.map(_ => 0.5).getOrElse(0.0)
      val foodScore = place.foods.headOption.map(_ => 0.5).getOrElse(0.0)
      creatureScore + foodScore
    } else {
      -1
    }
  }

  override def toString = field.map { line =>
    def who(place: Place) = place.creature.map(c => s"${(c.energy * 10).toInt.toHexString},${"%+f".format(c.replicationStrength).head}${(c.replicationStrength.abs * 10).toInt.toHexString}").getOrElse("-")
    def what(place: Place) = place.foods.size.toString
    Seq(who(_), what(_)).map(f => line.map(f(_).padTo(4, " ").mkString).mkString("|")).mkString("\n") + "\n" + "-" * 5 * line.size
  }.mkString("\n")
}
