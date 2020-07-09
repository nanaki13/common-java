package bon.jo

trait ViewCreator[V, C] extends ViewAggFactory[C] with ViewAgg[V, C]
