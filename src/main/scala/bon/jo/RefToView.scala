package bon.jo

import bon.jo.MethodBag.{Gett, Getters}
import bon.jo.Order.{Builder, ModOrder, OrderApi}
import bon.jo.ReflectUtil.ReflectApi
import bon.jo.ReflectUtil.TypeUtil

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.{universe => ru}

case class GetContext(getters: Gett,self : ru.InstanceMirror)
trait RefToView[A, V, C] extends ViewCreator[V, C] with ViewAgg[V, C] with AnyRefToViewProvider[V, C] {

  s: ReflectApi[A] =>


  val tpMap: Map[ClassTag[_], ViewFactory[Any, V, C]]

  val defaultView: ViewFactory[Any, V, C]
  implicit val viewSpec : ViewSpec

  def vList(listType: ru.Type): ViewFactory[Iterable[_], V, C] = ViewFactory[Iterable[_], V, C] {
    (value: Iterable[_]) => {
      val agg: C = RefToView.this ()
      val viewCerat = Try {
        createRefToView(Class.forName(listType.typeArgs.head.toString))
      } match {
        case Failure(_) => createRefToView(Class.forName(listType.typeArgs.head.toString.reverse.replaceFirst("\\.", "\\$").reverse))
        case Success(value) => value
      }

      def map(e: Any): V = viewCerat.toView(e).toViewBase

      value.map(map).foldLeft(agg)(_ :+ _).toViewBase
    }
  }

  def mapToView(field: String, value: Any)(implicit f: Map[String, ru.Type]): (String, Option[ViewFactory[Any, V, C]], Any) = {
    (field, {
      f.get(field
      ).flatMap(e => {
        if (!e.isJavaList) {
          val cTag = {
            val cName = (if (!e.toString.contains('.')) {
              "java.lang." + e.toString
            } else {
              e.toString
            }).replaceAll("""[\[_\]]""", "")
            reflect.ClassTag.apply(Class.forName(cName))
          }
          tpMap.get(cTag)
        } else if (e.isJavaList) {
          Option(vList(e).asInstanceOf[ViewFactory[Any, V, C]])
        } else {
          Option(defaultView)
        }

      })
    }, value)
  }


  def toView(e: A)(implicit spec: ViewSpec ): C = {
    val view: C = this.apply()
    implicit val f: Map[String, ru.Type] = fieldsAndType.toMap


    def localMapToView: (String, Any) => (String, Option[ViewFactory[Any, V, C]], Any) = mapToView(_, _)

    val gCtx = GetContext(extractGetters, mirror.reflect(e))
    val aspec = if(spec == NoSpec){
      spec.default(gCtx,this)
    }else{
      spec
    }
    gettersTransform(gCtx.getters, gCtx.self)(
      (g, o) => o
    ).filter(_._2 != None).map(localMapToView.tupled).foreach(e => {
      val vFavtory: ViewFactory[Any, V, C] = e._2.getOrElse(defaultView)
      view :+ (vFavtory(e._1, e._3))
    })
    view
  }
}

trait ViewSpec {
  def default(gCtx: GetContext,reflectApi: ReflectApi[_]) : ViewSpec = {

    val builderOrder = Builder.apply("root")
    val nameMap = reflectApi.gettersTransform(gCtx.getters,gCtx.self){
      (e,o) => e.get.map(_.name.toString)
    }
    nameMap.toList.sortBy{e => e._1}.zipWithIndex.foreach(a => {
      builderOrder.updateDynamic(a._1._1)(a._2)
      a._1._2 match {
        case e: Iterable[(String, Any)] =>
          explore(e)(builderOrder)
        case _ =>
      }

    })
    ViewSpec(builderOrder.r)
  }
  @tailrec
  def explore(l : Iterable[(String,Any)])(builderOrder : Builder) : Unit={
    l.toList.sortBy{e => e._1}.zipWithIndex.foreach(a => {
      builderOrder.updateDynamic(a._1._1)(a._2)
      a._1._2 match {
        case e: Iterable[(String, Any)] =>
          explore(e)(builderOrder)
        case _ =>
      }

    })
  }
  def specs: OrderApi
  def specs(order : OrderApi): ViewSpec = ViewSpec(order)
}
object ViewSpec{
  def apply(): ViewSpec =  ViewSpecImpl()
  def apply(order : OrderApi): ViewSpec =  ViewSpecImpl(order)
}
case class ViewSpecImpl(specs: OrderApi = NoOrder) extends ViewSpec
object NoSpec extends ViewSpec {
  override def specs: OrderApi = NoOrder
}
object NoOrder extends  NoOrder
trait NoOrder extends OrderApi{
  override def name: String = "NoOrder"

  override def index: Int = 0

  override def sub: Iterable[ModOrder] = Nil

  override def subMap: Map[String, ModOrder] = Map.empty

  override def order: Ordering[String] = throw new IllegalStateException("NoOrder don't have order")
}
trait Order {
  def name: String

  def index: Int

  def sub: Iterable[ModOrder]

  def haveChilds: Boolean = sub.isEmpty

  val subMap: Map[String, ModOrder] = sub.map(e => e.name -> e).toMap

  def order: Ordering[String] = subMap(_).index - subMap(_).index

}

trait ModifalbeOrderChild {
  me: Order =>
  var sub: List[Order]

  def +=(order: Order): Unit = sub = sub :+ order
}

object Order {
  type ModOrder = OrderApi with Order with ModifalbeOrderChild

  case class OrderImpl(name: String, index: Int, var sub: Iterable[ModOrder]) extends Order

  def apply(name: String, index: Int, sub: Iterable[ModOrder]): ModOrder = new OrderImpl(name, index, sub) with OrderApi with ModifalbeOrderChild

  case class BuilderImpl(override var root: ModOrder) extends Dynamic with Builder {


  }

  trait Builder extends Dynamic {
    protected var root: OrderApi with Order with ModifalbeOrderChild

    def updateDynamic(filed: String)(index: Int) = {
      root += Order(filed, index, Nil)
    }

    def selectDynamic(s: String): Builder = BuilderImpl(root.subMap(s))

    def r :OrderApi = root
  }

  trait OrderApi{
    def name: String

    def index: Int

    def sub: Iterable[ModOrder]

    def haveChilds: Boolean = sub.isEmpty

    def subMap: Map[String, ModOrder]

    def order: Ordering[String]
  }
  object Builder {
    def apply(name: String): Builder = BuilderImpl(Order(name, 0, Nil))
  }

  val b: Builder = Builder("b")
  b.nom = 1
  b.updateDynamic("class")(2)
  b.genre = 2
  b.genre.nom = 1

   b.r
}