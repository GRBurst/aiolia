package aiolia.helpers

object AutoId {
  def apply(start: Int = 0) = new AutoId(start)
}

class AutoId(var start: Int) extends Iterable[Int] {
  def nextId = {
    val current = start
    start += 1
    current
  }

  def iterator = new Iterator[Int] {
    def hasNext = true
    def next = nextId
  }

  def setIfHigher(ref: Int) {if(ref > start) start = ref + 1}
}
