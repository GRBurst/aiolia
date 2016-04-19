package aiolia.helpers

case class Random(seed: Any) {
  val r = new util.Random(seed.hashCode)

  def select[T](it:Iterable[T]):T = it.iterator.drop(r.nextInt(it.size)).next
  def select[T](it:Iterable[T], n:Int):Iterable[T] = {
    assert(it.size >= n)
    r.shuffle(it).take(n)
  }
}
