package bon.jo
import scala.reflect.runtime.{universe => ru}
object MethodBag{
  private case class GetSet(get: Iterable[ru.MethodSymbol], set: Iterable[ru.MethodSymbol]) extends GettSett

  private case class Getters(get: Iterable[ru.MethodSymbol]) extends Gett

  private case class Setters(set: Iterable[ru.MethodSymbol]) extends Sett

  trait Gett{
    def get: Iterable[ru.MethodSymbol]
  }
  trait Sett{
    def set: Iterable[ru.MethodSymbol]
  }
  trait GettSett extends Gett with Sett
  object Gett{
    def apply(get: Iterable[ru.MethodSymbol]): Gett =  Getters(get)
  }
  object Sett{
    def apply(set: Iterable[ru.MethodSymbol]): Sett =  Setters(set)
  }
  object GettSett{
    def apply(get: Iterable[ru.MethodSymbol], set: Iterable[ru.MethodSymbol]): GettSett = GetSet(get,set)
  }
}
