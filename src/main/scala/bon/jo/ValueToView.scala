package bon.jo

trait ValueToView[E, V] {
  def apply(value: E): V
}
