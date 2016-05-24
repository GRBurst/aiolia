package aiolia.app.zoo

trait Food {
  val energy: Double
}

object Apple extends Food {
  val energy = 0.5
  override def toString = s"Apple($energy)"
}
