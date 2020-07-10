package bon.jo

import java.{util => jut}

import bon.jo.Bridge.BridgeApi
import bon.jo.MethodBag.{Gett, Getters}
import bon.jo.ReflectUtil.{FromTypeReflect, JColFactory, TypeUtil}

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}


trait Bridge[I, O] extends BridgeApi[I, O] with JColFactory {


  val reflectUtilInput: ReflectUtil[I]

  val reflectUtilOutPut: ReflectUtil[O]

  import ReflectUtil.BridgeType

  protected def createBridge[I, O](implicit from: ru.TypeTag[I], to: ru.TypeTag[O]): BridgeType = {
    createBridge[O](reflectUtilInput.extractGetters)
  }

  protected def createBridge[O](getters: Gett)(implicit to: ru.TypeTag[O]): BridgeType = {
    val setByName: Map[String, ru.MethodSymbol] = reflectUtilOutPut
      .extractSetters
      .set
      .map(e => (reflectUtilInput.fieldName(e), e)).toMap

    val getByName: Map[String, ru.MethodSymbol] =
      getters
        .get
        .map(e => (reflectUtilInput.fieldName(e), e)).toMap

    val setToGet: BridgeType = setByName.map(e => (e._1, (e._2, getByName.get(e._1))))
    setToGet
  }

  protected def createBridge(in: ru.Type, out: ru.Type): BridgeType = {
    val setByName: Map[String, ru.MethodSymbol] =
      ReflectUtil.extractSetters(out)
        .set
        .map(e => (reflectUtilInput.fieldName(e), e)).toMap

    val getByName: Map[String, ru.MethodSymbol] =
      ReflectUtil.extractGetters(in)
        .get
        .map(e => (reflectUtilInput.fieldName(e), e)).toMap

    val setToGet: BridgeType = setByName.map(e => (e._1, (e._2, getByName.get(e._1))))
    setToGet
  }


  val mIn: ru.Mirror = reflectUtilInput.mirror
  val mOut: ru.Mirror = reflectUtilOutPut.mirror

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
                case Failure(exception) => println(exception)
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

  override def cp(in: I, out: O): Unit = {
    cpAny(in, out)(createBridge(reflectUtilInput.tpe,reflectUtilOutPut.tpe))
  }
}


object Bridge {
  def apply[O](value: ReflectUtil[O]): MonoBridgeApi[O] = new BridgeImplFromRef[O,O](value,value) with MonoBridgeApi[O]


  trait BridgeApi[I, O] {
    def cp(in: I, out: O): Unit
  }

  trait MonoBridgeApi[IO] extends BridgeApi[IO, IO]

  trait MonoBridge[IO] extends Bridge[IO, IO] with MonoBridgeApi[IO]


  def apply[I, O]()(implicit from: ru.TypeTag[I], to: ru.TypeTag[O], fromC: scala.reflect.ClassTag[I], toC: scala.reflect.ClassTag[O]): BridgeApi[I, O] = {
    new BridgeImpl[I, O](from.tpe, to.tpe)
  }

  def apply[IO]()(implicit from: ru.TypeTag[IO], fromC: scala.reflect.ClassTag[IO]): MonoBridgeApi[IO] = new BridgeImpl[IO, IO](from.tpe, from.tpe) with MonoBridge[IO]

  private[this] class BridgeImpl[I, O]
  (val from: ru.Type, val to: ru.Type)(
    implicit val cfrom: scala.reflect.ClassTag[I],
    val cto: scala.reflect.ClassTag[O]) extends Bridge[I, O] {

    override val reflectUtilInput: ReflectUtil[I] = new FromTypeReflect[I](from)
    override val reflectUtilOutPut: ReflectUtil[O] = new FromTypeReflect[O](to)
  }
  private[this] class BridgeImplFromRef[I, O]( override val reflectUtilInput: ReflectUtil[I],
  override val reflectUtilOutPut : ReflectUtil[O]) extends  Bridge[I, O]


}





