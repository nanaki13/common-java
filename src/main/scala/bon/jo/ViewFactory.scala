package bon.jo

trait ViewFactory[E, V, C] extends ValueToView[E, V] {
  val viewAgg: ViewAgg[V, C]

  import viewAgg._

  def apply(name: String, value: E): V = {
    toLabel(name) ++ this (value)
  }
}

object ViewFactory {
  def apply[E, V, C](f: E =>  V)(implicit viewAggP: ViewAgg[V, C]): ViewFactoryImpl[E, V, C] = ViewFactoryImpl(f)

}