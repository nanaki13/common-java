package bon.jo

trait ViewApi[V, C] {
  def c: C

  def :+(e: V)(implicit adder: ViewAgg[V, C]): C = {
    adder.addToAgg(c, e)
    c
  }

  def toViewBase(implicit adder: ViewAgg[V, C]): V = {
    adder.toViewBase(c)
  }
}
