package bon.jo
import scala.reflect.runtime.{universe => ru}
object MethodBag{
  case class GetSet(get: Iterable[ru.MethodSymbol], set: Iterable[ru.MethodSymbol])

  case class Getters(get: Iterable[ru.MethodSymbol])

  case class Setters(set: Iterable[ru.MethodSymbol])
}
