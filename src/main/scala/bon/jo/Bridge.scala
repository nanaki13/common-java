package bon.jo

import java.{util => jut}

import bon.jo.Bridge.BridgeApi
import bon.jo.MethodBag.Getters
import bon.jo.ReflectUtil.{JColFactory, TypeUtil}

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}


trait Bridge[I, O] extends BridgeApi[I,O]{
  def cp(in: I, out: O): Unit

  implicit val from: ru.TypeTag[I]
  implicit val to: ru.TypeTag[O]
  implicit val cfrom: ClassTag[I]
  implicit val cto: ClassTag[O]
  val reflectUtilInput: ReflectUtil[I] = new ReflectUtil.ReflectUtilImpl[I]

  val reflectUtilOutPut: ReflectUtil[O] = new ReflectUtil.ReflectUtilImpl[O]

  import ReflectUtil.BridgeType

  protected def createBridge[I, O](implicit from: ru.TypeTag[I], to: ru.TypeTag[O]): BridgeType = {
    createBridge[O](reflectUtilInput.extractGetters)
  }

  protected def createBridge[O](getters: Getters)(implicit to: ru.TypeTag[O]): BridgeType = {
    val setByName: Map[String, ru.MethodSymbol] = reflectUtilOutPut
      .extractSetters
      .set
      .map(e => (e.name.toString.substring(3).toLowerCase, e)).toMap

    val getByName: Map[String, ru.MethodSymbol] =
      getters
        .get
        .map(e => (e.name.toString.substring(3).toLowerCase, e)).toMap

    val setToGet: BridgeType = setByName.map(e => (e._1, (e._2, getByName.get(e._1))))
    setToGet
  }

  protected def createBridge(in: ru.Type, out: ru.Type): BridgeType = {
    val setByName: Map[String, ru.MethodSymbol] =
      ReflectUtil.extractSetters(out)
        .set
        .map(e => (e.name.toString.substring(3).toLowerCase, e)).toMap

    val getByName: Map[String, ru.MethodSymbol] =
      ReflectUtil.extractGetters(in)
        .get
        .map(e => (e.name.toString.substring(3).toLowerCase, e)).toMap

    val setToGet: BridgeType = setByName.map(e => (e._1, (e._2, getByName.get(e._1))))
    setToGet
  }
}


object Bridge {

  trait BridgeApi[I,O]{
    def cp(in: I, out: O): Unit
  }
  trait MonoBridgeApi[IO] extends BridgeApi[IO,IO]
  trait MonoBridge[IO] extends Bridge[IO,IO] with MonoBridgeApi[IO]


  def apply[I, O]()(implicit from: ru.TypeTag[I], to: ru.TypeTag[O], fromC: scala.reflect.ClassTag[I], toC: scala.reflect.ClassTag[O]): BridgeApi[I, O] = new BridgeImpl[I, O]()
  def apply[IO]()(implicit from: ru.TypeTag[IO], fromC: scala.reflect.ClassTag[IO]): MonoBridgeApi[IO] = new BridgeImpl[IO, IO]()(from,from,fromC,fromC) with MonoBridge[IO]

  private[this] class BridgeImpl[I, O]
  (implicit val from: ru.TypeTag[I], val to: ru.TypeTag[O],
   val cfrom: scala.reflect.ClassTag[I],
   val cto: scala.reflect.ClassTag[O]) extends Bridge[I, O] with JColFactory {

    import ReflectUtil.BridgeType


    val mIn: ru.Mirror = ru.runtimeMirror(cfrom.runtimeClass.getClassLoader)
    val mOut: ru.Mirror = ru.runtimeMirror(cto.runtimeClass.getClassLoader)


    private def cpAny(in: Any, ot: Any)(bridge: BridgeType): Unit = {
      val imFrom = mIn.reflect(in)
      val imTo = mOut.reflect(ot)
      bridge.foreach(e => {
        e._2._2.foreach(get => {
          val setterParam = e._2._1.paramLists.head.head.asTerm.info
          val fromGet = imFrom.reflectMethod(get)()
          if (setterParam.isImmutable) {
            imTo.reflectMethod(e._2._1)(fromGet)
          } else if (setterParam.isJavaList) {
            val setColGenType = setterParam.typeArgs.head
            val getColGenType = get.returnType.typeArgs.head
            if (!setColGenType.isImmutable) {
              val jList = imFrom.reflectMethod(get)().asInstanceOf[jut.List[_]]
              if (jList != null) {
                val colBridge = createBridge(getColGenType, setColGenType)
                val colOut = jArrayList[Any]
                val construct = setColGenType
                  .members
                  .filter(_.isConstructor)
                  .map(_.asMethod)
                  .filter(_.paramLists == List(Nil)).head
                Try(jList.forEach { e =>
                  val outColElment = mOut.reflectClass(setColGenType.typeSymbol.asClass).reflectConstructor(construct)()
                  cpAny(e, outColElment)(colBridge)
                  colOut.add(outColElment)
                }) match {
                  case Failure(exception) =>
                  case Success(_) => imTo.reflectMethod(e._2._1)(colOut)
                }
              }
            }
          } else {
            println(s"not handle : $setterParam")
          }

        })
      })
    }

    def cp(in: I, out: O): Unit = {
      cpAny(in, out)(createBridge[I, O])
    }
  }

}





