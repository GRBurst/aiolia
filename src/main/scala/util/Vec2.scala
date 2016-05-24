package aiolia.util

case class Vec2(x: Int, y: Int) {
  def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  def -(that: Vec2) = Vec2(x - that.x, y - that.y)

  def distance(that: Vec2) = Math.sqrt((this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y))
  def length = Math.sqrt(x * x + y * y)
  def angle = Math.atan2(y, x)
  def isZero = (x == 0 && y == 0)

  override def toString = s"($x,$y)"
}
