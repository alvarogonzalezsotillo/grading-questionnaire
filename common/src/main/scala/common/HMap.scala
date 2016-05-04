package common

import java.util.Date


/**
 * Created by alvaro on 1/05/16.
 */

trait StringFromObjectInObjectName{
  override def toString = getClass().getName().dropWhile(_ != '$').drop(1).takeWhile(_ != '$')
}

trait HKey[T] extends StringFromObjectInObjectName{
  def apply(hmap: HMap): T = hmap(this).get

  def apply(__ : Int = 0)(implicit hmap: HMap): T = hmap(this).get

  def <--(v: T)(implicit hmap: HMap): HMap = hmap.updated(this, v)
}

trait HMap {
  protected val map: Map[HKey[_], Any]

  def apply[T](key: HKey[T]): Option[T] = map.get(key).asInstanceOf[Option[T]]

  def apply( hmap: HMap ) : HMap = this ++ hmap

  def updated[T](k: HKey[T], v: T): HMap

  def ++( hmap: HMap ) : HMap

  override def toString = map.toString
}

object HMap {

  private class HMapI(val map: Map[HKey[_], Any]) extends HMap {
    def updated[T](k: HKey[T], v: T): HMap = new HMapI(map.updated(k, v))
    def ++( hmap: HMap ) = new HMapI( map ++ hmap.asInstanceOf[HMapI].map )
  }

  def apply(): HMap = new HMapI(Map())
}


object HMapSample extends App {

  object importance extends HKey[Int]

  implicit val hmap = HMap()

  val plantilla = HMap().updated(importance,1)

  object name extends HKey[String]

  object address extends HKey[String]

  object dob extends HKey[Date]

  object emails extends HKey[List[String]]


  val hmap2 = (address <-- "a")(dob <-- new Date)(name <-- "pepe" )(emails <-- "a@a.com" :: "b@b.com" :: Nil )

  println(hmap)
  println(hmap2)

  println(hmap(name))
  println(hmap(address))
  println(address(hmap))
  println(address())
  println(dob())

}