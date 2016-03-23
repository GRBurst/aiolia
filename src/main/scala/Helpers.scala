package aiolia.helpers

class AutoId(var start: Int = 0) {
  def nextId = {
    val current = start
    start += 1
    current
  }
}
