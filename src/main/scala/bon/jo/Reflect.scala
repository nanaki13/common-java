package bon.jo

import java.{lang => jlg, util => jut}

import bon.jo.JeuxVideo.Genre
import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}

object Reflect extends App with JColFactory {
  val copieur = new BridgeImpl[JeuxVideo, JeuxVideo]
  test




  def javaMethod(e: ru.Symbol): Boolean = (e.isMethod && e.isJava)

  def javaGet(e: ru.Symbol): Boolean = javaMethod(e) && e.asMethod.name.toString.startsWith("get")

  def javaSet(e: ru.Symbol): Boolean = javaMethod(e) && e.asMethod.name.toString.startsWith("set")

  def test(implicit t: ru.TypeTag[JeuxVideo]): Unit = {

    val j = new JeuxVideo
    val genre = jArrayList[Genre]

    genre add new Genre
    (genre get 0) setName "Action"
    j setGenre genre
    j.setName("test")
    val j2 = new JeuxVideo
    cp(j, j2)
    println(j2.getName)
    println(j2.getGenre.get(0).getName)

  }


  def cp(from: JeuxVideo, to: JeuxVideo)(implicit t: ru.TypeTag[JeuxVideo]) = {

    copieur.cp(from, to)(copieur.createBridge[JeuxVideo,JeuxVideo])


  }


  case class GetSet(get: Iterable[ru.MethodSymbol], set: Iterable[ru.MethodSymbol])

  case class Getters(get: Iterable[ru.MethodSymbol])

  case class Setters(set: Iterable[ru.MethodSymbol])

  object ReflectUtil {
    type BridgeType = Map[String, (ru.MethodSymbol, Option[ru.MethodSymbol])]

    def extractGetSet(tpe: ru.Type): GetSet = GetSet(tpe.members.filter(javaGet).map(_.asMethod)
      , tpe.members.filter(javaSet).map(_.asMethod))

    def extractGetters(tpe: ru.Type): Getters = Getters(tpe.members.filter(javaGet).map(_.asMethod)
    )

    def extractSetters(tpe: ru.Type): Setters = Setters(tpe.members.filter(javaSet).map(_.asMethod))


  }

  trait ReflectUtil[T] {

    import ReflectUtil.BridgeType

    implicit val tt: ru.TypeTag[T]

    def extractGetSet: GetSet = ReflectUtil.extractGetSet(tt.tpe)

    def extractGetters: Getters = ReflectUtil.extractGetters(tt.tpe)


    def extractSetters: Setters = ReflectUtil.extractSetters(tt.tpe)



  }

  class ReflectUtilImpl[T](implicit val tt: ru.TypeTag[T]) extends ReflectUtil[T] {

    import ReflectUtil.BridgeType




  }

  trait Bridge {

    import ReflectUtil.BridgeType

    def createBridge[I, O](implicit from: ru.TypeTag[I], to: ru.TypeTag[O]): BridgeType = {
       createBridge[O](ReflectUtil.extractGetters(from.tpe))
    }
    def createBridge[O](getters: Getters)(implicit to: ru.TypeTag[O]): BridgeType = {
      val setByName: Map[String, ru.MethodSymbol] = (new ReflectUtilImpl[O])
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

    def createBridge(in: ru.Type, out: ru.Type): BridgeType = {
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

  implicit class TypeUtil(val t: ru.Type) {
    def isImmutable = SimpleType.simpleType.contains(t)

    def isJavaList = {
      t <:< ru.typeTag[jut.List[_]].tpe
    }
  }

  class BridgeImpl[I, O](implicit val from: ru.TypeTag[I], val to: ru.TypeTag[O], fromC: scala.reflect.ClassTag[I], toC: scala.reflect.ClassTag[O]) extends Bridge with JColFactory {

    import ReflectUtil.BridgeType


    val mIn = ru.runtimeMirror(fromC.runtimeClass.getClassLoader)
    val mOut = ru.runtimeMirror(toC.runtimeClass.getClassLoader)

    def reflectUtilInput: ReflectUtil[I] = new ReflectUtilImpl[I]

    def reflectUtilOutPut: ReflectUtil[O] = new ReflectUtilImpl[O]

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

    def cp(in: I, out: O)(implicit bridge: BridgeType) = cpAny(in, out)(bridge)
  }




}


trait JColFactory {
  def jArrayList[A] = new jut.ArrayList[A]()

  def jLinkedList[A] = new jut.LinkedList[A]()
}

object JColFactory extends JColFactory

object SimpleType {
  val simpleType = Set(
    ru.typeTag[jlg.Long].tpe,
    ru.typeTag[jlg.Integer].tpe,
    ru.typeTag[jlg.Double].tpe,
    ru.typeTag[jlg.Float].tpe,
    ru.typeTag[jlg.String].tpe,
    ru.typeTag[jlg.Boolean].tpe,
    ru.typeTag[java.time.LocalDate].tpe,
    ru.typeTag[java.time.LocalDateTime].tpe,
    ru.typeTag[java.time.Instant].tpe
  )
  val jColType = Set(
    ru.typeTag[jut.Collection[_]].tpe.erasure
  )
}