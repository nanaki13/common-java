package bon.jo

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import bon.jo.ReflectUtil.ReflectApi
import bon.jo.SwingRefToView.Tr
import javax.swing.{JComponent, JLabel, JPanel, JTextField}

import scala.reflect.ClassTag
trait SwingRefToView[A] extends RefToView[A, JComponent, JPanel] with ReflectApi[A] {
  override def addToAgg(a: JPanel, v: JComponent): Unit = a.add(v)

  override def toViewBase(parent: JPanel): JComponent = parent

  override def apply(): JPanel = new JPanel()


  override def toLabel(name: String): JComponent = new JLabel(name)

  override val tpMap: Map[ClassTag[_], ViewFactory[Any, JComponent, JPanel]] = SwingRefToView.tpMapBase.view.mapValues(
    toViewFactory
  ).toMap
  override val defaultView: ViewFactory[Any, JComponent, JPanel] = toViewFactory(SwingRefToView.defo)

  def toViewFactory(tr: Tr[_]): ViewFactory[Any, JComponent,JPanel] =
    ViewFactory[Any, JComponent,JPanel](tr.toFunction)
}

object SwingRefToView {


  def s(s: Any) = if (s != null) s.toString else ""

  implicit val numberRep: Tr[Number] = l => {
    println("number")
    new JTextField(s(l))
  }
  implicit val dtRep: Tr[LocalDate] = l => {
    println("number")
    new JTextField(s(l.format(DateTimeFormatter.ofPattern("MM YYYY"))))
  }
  implicit val stringRep: Tr[String] = l => {
    println("string")
    new JTextField(l)
  }
  val defo: Tr[Any] = l => {
    println("any")
    new JTextField(s(l))
  }

  trait Tr[A] {
    def apply(a: A): JComponent
    def toFunction : Any=>JComponent = a =>  apply(a.asInstanceOf[A])
  }

  val tpMapBase: Map[ClassTag[_], Tr[_]] = {
    Map(
      implicitly[ClassTag[Number]] -> {
        implicitly[Tr[Number]]
      },
      implicitly[ClassTag[Integer]] -> {
        implicitly[Tr[Number]]
      },
      implicitly[ClassTag[Long]] -> {
        implicitly[Tr[Number]]
      },
      implicitly[ClassTag[Float]] -> {
        implicitly[Tr[Number]]
      },
      implicitly[ClassTag[Number]] -> {
        implicitly[Tr[Number]]
      },
      implicitly[ClassTag[String]] -> {
        implicitly[Tr[String]]
      },
        implicitly[ClassTag[LocalDate]] -> {
        implicitly[Tr[LocalDate]]
      }
    )
  }
}