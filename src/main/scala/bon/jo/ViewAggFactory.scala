package bon.jo

trait ViewAggFactory[C] {
  def apply(): C
}
