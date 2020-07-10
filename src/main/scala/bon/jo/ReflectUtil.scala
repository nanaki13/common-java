package bon.jo

import java.{lang => jlg, util => jut}

import bon.jo.Bridge.MonoBridgeApi
import bon.jo.MethodBag.{Gett, GettSett, Sett}
import bon.jo.ReflectUtil.TypeUtil
import javax.swing.{JComponent, JPanel}

import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

object ReflectUtil {

  implicit class TypeUtil(val t: ru.Type) {
    def isImmutable = SimpleType.javaImmutable.contains(t)

    def isJavaList = {
      t <:< ru.typeTag[jut.List[_]].tpe
    }

    def firstGenType = t.typeArgs.head
  }

  trait JColFactory {
    def jArrayList[A] = new jut.ArrayList[A]()

    def jLinkedList[A] = new jut.LinkedList[A]()
  }

  def fieldName(m: ru.MethodSymbol): String = {
    m.name.toString.substring(3, m.name.toString.length).toLowerCase
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

  def extractGetSet(tpe: ru.Type): GettSett = GettSett(tpe.members.filter(javaGet).map(_.asMethod)
    , tpe.members.filter(javaSet).map(_.asMethod))

  def extractGetters(tpe: ru.Type): Gett = Gett(tpe.members.filter(javaGet).map(_.asMethod)
  )

  def extractSetters(tpe: ru.Type): Sett = Sett(tpe.members.filter(javaSet).map(_.asMethod))

  class FromTypeTagReflect[T](implicit val tt: ru.TypeTag[T], val cTag: ClassTag[T]) extends ReflectUtil[T] {
    override val tpe = tt.tpe
    override val mirror: ru.Mirror = ru.runtimeMirror(cTag.runtimeClass.getClassLoader)
  }

  class FromTypeReflect[T](val tt: ru.Type)(implicit val cTag: ClassTag[T]) extends ReflectUtil[T] {
    override val tpe = tt
    override val mirror: ru.Mirror = ru.runtimeMirror(cTag.runtimeClass.getClassLoader)
  }

  class JavaReflectUtilImpl[T](class_ : Class[T]) extends ReflectUtil[T] {
    override val mirror: ru.Mirror = ru.runtimeMirror(class_.getClassLoader)
    override val tpe: ru.Type = mirror.staticClass(class_.getName).selfType
    override implicit val cTag: ClassTag[T] = ClassTag[T](class_)
  }

  trait ReflectApi[T] {
    def cp(in: T, out: T): Unit

    def extractGetSet: GettSett

    def extractGetters: Gett

    def extractSetters: Sett

    def fields: Iterable[String]

    def fieldsAndType: Iterable[(String, ru.Type)]

    def asMap(t: T): Map[String, Any]

    def asFullMap(t: T): Iterable[(String, Any)]

    def mirror: ru.Mirror

    def gettersTransform(getters: Gett, o: ru.InstanceMirror)(
      ifNotImmutable: (Gett, Any) => Any
    ): Iterable[(String, Any)]

    implicit val cTag: ClassTag[T]
  }

  //Java API
  def create[T](class_ : Class[T]): SwingRefToView[T] with ReflectApi[T] = {
    new JavaReflectUtilImpl[T](class_) with ReflectApi[T] with SwingRefToView[T] {
      override def createRefToView(classs: Class[_]): RefToView[Any, JComponent, JPanel] = {
        new JavaReflectUtilImpl[Any](classs.asInstanceOf[Class[Any]]) with ReflectApi[Any] with SwingRefToView[Any] {
          override def createRefToView(classs: Class[_]): RefToView[Any, JComponent, JPanel] = {
            create[Any](classs.asInstanceOf[Class[Any]])
          }

          override implicit val viewSpec: ViewSpec = NoSpec
        }
      }

      override implicit val viewSpec: ViewSpec = NoSpec
    }
  }

}

trait ReflectUtil[T] extends MonoBridgeApi[T] {


  val tpe: ru.Type
  val mirror: ru.Mirror
  protected implicit val cTag: ClassTag[T]

  val selfBridge: MonoBridgeApi[T] = Bridge(this)


  override def cp(in: T, out: T): Unit = selfBridge.cp(in, out)

  def extractGetSet: GettSett = ReflectUtil.extractGetSet(tpe)

  def extractGetters: Gett = ReflectUtil.extractGetters(tpe)


  def extractSetters: Sett = ReflectUtil.extractSetters(tpe)

  def fieldName(ms: ru.MethodSymbol): String = ReflectUtil.fieldName(ms)

  def asMap(t: T): Map[String, Any] = {
    val instanceMirror = mirror.reflect[T](t)
    extractGetters.get.map(m => (ReflectUtil.fieldName(m).toLowerCase, instanceMirror.reflectMethod(m).apply())).toMap
  }

  def asFullMap(t: T): Iterable[(String, Any)] = {
    val instanceMirror: ru.InstanceMirror = mirror.reflect[T](t)
    gettersToMap(extractGetters, instanceMirror)
  }


  def gettersToMap(getters: Gett, o: ru.InstanceMirror): Iterable[(String, Any)] = {
    gettersTransform(getters, o)((gett, oo) => gettersToMap(gett, mirror.reflect[Any](oo)))
  }

  def gettersTransform(getters: Gett, o: ru.InstanceMirror)(
    ifNotImmutable: (Gett, Any) => Any
  ): Iterable[(String, Any)] = {
    getters.get.map(m => (ReflectUtil.fieldName(m).toLowerCase, {
      println(s"${m.name} , $o")
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
          else if (getColGenType.isJavaList) {
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

  def fields: Iterable[String] = extractGetters.get.map(fieldName)

  def fieldsAndType: Iterable[(String, ru.Type)] = extractGetters.get.map(e => (fieldName(e), e.returnType))


}


























