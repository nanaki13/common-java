package bon.jo

import scala.reflect.ClassTag

trait AnyRefToViewProvider[V, C] {
  def createRefToView(classs: Class[_]): RefToView[Any, V, C]

  def createRefToView[T]()(implicit classTag: ClassTag[T]): RefToView[Any, V, C] = createRefToView(classTag.runtimeClass)
}
