package bon.jo

case class ViewFactoryImpl[E, V, C](f: E =>  V)(override implicit val viewAgg: ViewAgg[V, C]) extends ViewFactory[E, V, C] {


  override def apply(value: E): V = f(value)
}
