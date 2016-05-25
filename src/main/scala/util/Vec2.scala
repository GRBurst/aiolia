package aiolia.util

case class Vec2(x: Int, y: Int) {
  def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  def -(that: Vec2) = Vec2(x - that.x, y - that.y)
  def *(s: Int) = Vec2(x * s, y * s)
  def /(s: Int) = Vec2(x / s, y / s)

  def unary_- = Vec2(-x, -y)

  def distance(that: Vec2) = Math.sqrt((this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y))
  def length = Math.sqrt(x * x + y * y)
  def angle = Math.atan2(y, x)
  def isZero = (x == 0 && y == 0)

  def rotate(angle: Double) = {
    assert(x.abs == 1 || x == 0)
    assert(y.abs == 1 || y == 0)

    val res = Vec2(
      x = Math.cos(this.angle + angle).signum,
      y = Math.sin(this.angle + angle).signum
    )

    assert(res.x.abs == 1 || res.x == 0)
    assert(res.y.abs == 1 || res.y == 0)
    res
  }

  override def toString = s"($x,$y)"
}

object Vec2 {
  val zero = Vec2(0, 0)

  // x : positive right, negative left
  // y : positive up, negative down
  val left = Vec2(-1, 0)
  val down = Vec2(0, -1)
  val right = Vec2(1, 0)
  val up = Vec2(0, 1)
  val leftDown = left + down
  val rightDown = right + down
  val leftUp = left + up
  val rightUp = right + up
}