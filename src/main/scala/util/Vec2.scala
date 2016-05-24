package aiolia.util

case class Vec2(x: Int, y: Int) {
  def +(that:Vec2) = Vec2(x + that.x, y + that.y)
  def -(that:Vec2) = Vec2(x - that.x, y - that.y)

  def distance(that:Vec2) = Math.sqrt(Math.pow(this.x - that.y, 2) + Math.pow(this.y - that.y, 2))
  def angle = Math.atan2(y, x);

  override def toString = s"($x,$y)"
}
