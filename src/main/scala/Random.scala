package aiolia.helpers

case class Random(seed: Any) {
  val r = new util.Random(seed.hashCode)

  def select[T](it: Iterable[T]): T = it.iterator.drop(r.nextInt(it.size)).next

  //TODO: def select[I[T] <: Iterable[T]](it: I[T], n: Int): I[T] = {
  // to return the same type of collection as the input
  def select[T](it: Iterable[T], n: Int): Iterable[T] = {
    assert(n <= it.size)
    r.shuffle(it).take(n)
  }

  def selectMinOne[T](it: Iterable[T], max: Int): Iterable[T] = select(it, n = nextInt(1, max))
  def selectMinOne[T](it: Iterable[T]): Iterable[T] = select(it, n = nextInt(1, it.size + 1))
  def selectOpt[T](it: Iterable[T]): Option[T] = if (it.isEmpty) None else Some(select(it))
  def selectOpt[T](it: Iterable[T], n: Int): Option[Iterable[T]] = if (it.size < n) None else Some(select(it, n))

  def nextInt(from: Int, until: Int): Int = {
    assert(from >= 0)
    assert(until > from)
    from + r.nextInt(until - from)
  }
}
