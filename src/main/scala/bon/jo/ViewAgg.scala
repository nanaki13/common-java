package bon.jo

trait ViewAgg[V, C] extends ViewAggFactory[C] {

  implicit val add: ViewAgg[V, C] = this

  implicit class ViewAggApi(val c: C) extends ViewApi[V, C]

  implicit class VieApi(val v: V) {
    def ++(o: V): V = assemble(v, o)
  }

  def addToAgg(c: C, v: V): Unit

  def toViewBase(agg: C): V

  def assemble(v1: V, v2: V): V = {
    val cont = this ()
    cont :+ v1
    cont :+ v2
    cont.toViewBase
  }

  def toLabel(name: String): V


}
