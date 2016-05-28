package aiolia.util

case class Vec2(x: Int, y: Int) {
  def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  def -(that: Vec2) = Vec2(x - that.x, y - that.y)

  def +(s: Int) = Vec2(x + s, y + s)
  def -(s: Int) = Vec2(x - s, y - s)
  def *(s: Int) = Vec2(x * s, y * s)
  def /(s: Int) = Vec2(x / s, y / s)

  def unary_- = Vec2(-x, -y)

  def lengthSq = x * x + y * y
  def length = Math.sqrt(lengthSq)
  def distanceSq(that: Vec2) = (this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y)
  def distance(that: Vec2) = Math.sqrt(distanceSq(that))

  // def angle = Math.atan2(y, x)
  def angle = Vec2.angleCacheLookup(this)

  def isZero = (x == 0 && y == 0)

  def rotate(angle: Double) = {
    assert(x.abs == 1 || x == 0)
    assert(y.abs == 1 || y == 0)

    def thirds(x: Double) = if (x < -0.3) -1 else if (x > 0.3) 1 else 0
    val res = Vec2(
      x = thirds(Math.cos(this.angle + angle)),
      y = thirds(Math.sin(this.angle + angle))
    )

    assert(res.x.abs == 1 || res.x == 0)
    assert(res.y.abs == 1 || res.y == 0)
    res
  }

  def until(that: Vec2): Iterable[Vec2] = {
    Stream.tabulate(that.x - this.x, that.y - this.y)((x, y) => Vec2(this.x + x, this.y + y)).flatten
  }

  def untilByY(that: Vec2): Iterable[Vec2] = {
    Stream.tabulate(that.y - this.y, that.x - this.x)((y, x) => Vec2(this.x + x, this.y + y)).flatten
  }

  def to(that: Vec2): Iterable[Vec2] = {
    Stream.tabulate(that.x - this.x + 1, that.y - this.y + 1)((x, y) => Vec2(this.x + x, this.y + y)).flatten
  }

  override def toString = s"Vec2($x,$y)"
}

object Vec2 {
  val angleCacheRadius = 10
  val angleCache = (for (v @ Vec2(x, y) <- -Vec2(1, 1) * angleCacheRadius to Vec2(1, 1) * angleCacheRadius) yield {
    Math.atan2(y, x)
  }).toArray
  def angleCacheLookup(v: Vec2) = angleCache((v.x + angleCacheRadius) * (2 * angleCacheRadius + 1) + v.y + angleCacheRadius)
  //TODO: test cases:
  assert(angleCacheLookup(Vec2(1, 2)) == Math.atan2(2, 1), s"${angleCacheLookup(Vec2(1, 2))} != ${Math.atan2(2, 1)}")
  assert(angleCacheLookup(Vec2(-5, -3)) == Math.atan2(-3, -5), s"${angleCacheLookup(Vec2(-5, -3))} != ${Math.atan2(-3, -5)}")
  assert(angleCacheLookup(Vec2(-5, 3)) == Math.atan2(3, -5))

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
