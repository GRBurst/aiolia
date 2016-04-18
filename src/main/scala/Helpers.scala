package aiolia.helpers

object AutoId {
  def apply(start: Int = 0) = new AutoId(start)
}

class AutoId(var start:Int) {
  def nextId = {
    val current = start
    start += 1
    current
  }
}
