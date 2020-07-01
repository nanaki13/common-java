import bon.jo.{Bridge, JeuxVideo}
import bon.jo.JeuxVideo.Genre
import bon.jo.ReflectUtil.{JColFactory, ReflectUtilImpl}

import scala.reflect.runtime.{universe => ru}
import org.scalatest.funsuite.AnyFunSuite

class ReflectTest extends AnyFunSuite{


  val copieur = Bridge[JeuxVideo, JeuxVideo]()


  def testCP(implicit t: ru.TypeTag[JeuxVideo]): Unit = {
    val j = new JeuxVideo
    val genre = JColFactory.jArrayList[Genre]
    genre add new Genre
    (genre get 0) setName "Action"
    j setGenre genre
    j.setName("test")
    val j2 = new JeuxVideo
    copieur.cp(j, j2)
    println(j2.getName)
    println(j2.getGenre.get(0).getName)
    assert( j == j2)

  }

  val ref = new ReflectUtilImpl[JeuxVideo]



  test("a copied object is equals to the source"){
    testCP
  }

  test(""){
    val j = new JeuxVideo
    val genre = JColFactory.jArrayList[Genre]
    genre add new Genre
    (genre get 0) setName "Action"
    j setGenre genre
    j.setName("test")
    println(ref.asFullMap(j))
    assert(ref.asMap(j).keys == Set("name","genre","sorti","class"))

  }
}