package bon.jo

import bon.jo.MethodBag.{GetSet, Getters, Setters}

import scala.reflect.runtime.{universe => ru}
import java.{lang => jlg, util => jut}

import ReflectUtil.TypeUtil

import scala.reflect.ClassTag
import scala.jdk.CollectionConverters._

object ReflectUtil {

  implicit class TypeUtil(val t: ru.Type) {
    def isImmutable = SimpleType.javaImmutable.contains(t)

    def isJavaList = {
      t <:< ru.typeTag[jut.List[_]].tpe
    }
  }

  trait JColFactory {
    def jArrayList[A] = new jut.ArrayList[A]()

    def jLinkedList[A] = new jut.LinkedList[A]()
  }

  def fieldName(m: ru.MethodSymbol): String = {
    m.name.toString.substring(3, m.name.toString.length)
  }

  object JColFactory extends JColFactory

  object SimpleType {
    val javaImmutable = Set(
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


  def javaMethod(e: ru.Symbol): Boolean = (e.isMethod && e.isJava)

  def javaGet(e: ru.Symbol): Boolean = javaMethod(e) && e.asMethod.name.toString.startsWith("get")

  def javaSet(e: ru.Symbol): Boolean = javaMethod(e) && e.asMethod.name.toString.startsWith("set")


  type BridgeType = Map[String, (ru.MethodSymbol, Option[ru.MethodSymbol])]

  def extractGetSet(tpe: ru.Type): GetSet = GetSet(tpe.members.filter(javaGet).map(_.asMethod)
    , tpe.members.filter(javaSet).map(_.asMethod))

  def extractGetters(tpe: ru.Type): Getters = Getters(tpe.members.filter(javaGet).map(_.asMethod)
  )

  def extractSetters(tpe: ru.Type): Setters = Setters(tpe.members.filter(javaSet).map(_.asMethod))

  class ReflectUtilImpl[T](implicit val tt: ru.TypeTag[T],val cTag : ClassTag[T]) extends ReflectUtil[T] {
    override val tpe = tt.tpe
    override val mirror: ru.Mirror = ru.runtimeMirror(cTag.runtimeClass.getClassLoader)
  }

  class JavaReflectUtilImpl[T](class_ : Class[T]) extends ReflectUtil[T] {
    override val mirror: ru.Mirror = ru.runtimeMirror(class_.getClassLoader)
    override val tpe: ru.Type = mirror.staticClass(class_.getName).selfType
    override implicit val cTag: ClassTag[T] = ClassTag[T](class_)
  }

  //Java API
  def create[T](class_ : Class[T]): ReflectUtil[T] = {
    new JavaReflectUtilImpl[T](class_)
  }

}

trait ReflectUtil[T] {


  val tpe: ru.Type
  val mirror: ru.Mirror
  implicit val cTag : ClassTag[T]
  def extractGetSet: GetSet = ReflectUtil.extractGetSet(tpe)

  def extractGetters: Getters = ReflectUtil.extractGetters(tpe)


  def extractSetters: Setters = ReflectUtil.extractSetters(tpe)


  def asMap(t: T): Map[String, Any] = {
    val instanceMirror = mirror.reflect[T](t)
    extractGetters.get.map(m => (ReflectUtil.fieldName(m).toLowerCase, instanceMirror.reflectMethod(m).apply())).toMap
  }

  def asFullMap(t: T): Iterable[(String, Any)] = {
    val instanceMirror: ru.InstanceMirror = mirror.reflect[T](t)
    gettersToMap(extractGetters, instanceMirror)
  }



  def gettersToMap(getters: Getters, o: ru.InstanceMirror): Iterable[(String, Any)] = {
    gettersTransform(getters, o)((gett, oo) => gettersToMap(gett, mirror.reflect[Any](oo)))
  }

  def gettersTransform(getters: Getters, o: ru.InstanceMirror)(
    ifNotImmutable: (Getters, Any) => Any
  ): Iterable[(String, Any)] = {
    getters.get.map(m => (ReflectUtil.fieldName(m).toLowerCase, {
      println(s"${m.name}")
      val getted = o.reflectMethod(m).apply()
      println(s"${m.name} = $getted")
      if (!m.returnType.isImmutable && !m.returnType.isJavaList && !(m.returnType <:< ru.typeTag[jlg.Class[_]].tpe))
        ifNotImmutable(ReflectUtil.extractGetters(m.returnType), getted)
      else if (m.returnType.isJavaList) {
        val getColGenType = m.returnType.typeArgs.head
        val getterOfColGen = ReflectUtil.extractGetters(getColGenType)
        getted.asInstanceOf[jut.List[Any]].asScala.map(e => {
          if (!getColGenType.isImmutable && !(getColGenType <:< ru.typeTag[jlg.Class[_]].tpe))
            ifNotImmutable(getterOfColGen, e)
          else if (m.returnType.isJavaList) {
            println("List of list not handle")
          } else {
            e
          }
        })
      } else {
        getted
      }
    }))
  }


  def asFullMap(a: Any)(implicit t: ru.Type, rMirror: ru.Mirror): Iterable[(String, Any)] = {
    val instanceMirror = rMirror.reflect[Any](a)
    gettersToMap(ReflectUtil.extractGetters(t), instanceMirror)

  }
}