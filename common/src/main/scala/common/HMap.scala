package common

import java.util.Date


/**
 * Created by alvaro on 1/05/16.
 */

trait StringFromObjectInObjectName{
  override def toString = getClass().
    getName().
    reverse.
    dropWhile(_ != '$').
    drop(1).
    takeWhile(_ != '$').
    reverse
}

trait HKey[T] extends StringFromObjectInObjectName{
  def apply(hmap: HMap): T = hmap(this).get

  def apply(__ : Int = 0)(implicit hmap: HMap): T = apply(hmap)

  def <--(v: Option[T])(implicit hmap: HMap): HMap = hmap.updated(this,v)
  def <--(v: T)(implicit hmap: HMap): HMap = hmap.updated(this, v)
}

sealed trait HMap {
  protected val map: Map[HKey[_], Any]

  def apply[T](key: HKey[T]): Option[T] = map.get(key).asInstanceOf[Option[T]]

  def apply( hmap: HMap ) : HMap = this ++ hmap

  def updated[T](k: HKey[T], v: T): HMap

  def deleted[T](k: HKey[T]): HMap

  def updated[T](k: HKey[T], ov: Option[T] ) : HMap = ov match{
    case Some(v) => updated(k,v)
    case None => deleted(k)
  }


  def ++( hmap: HMap ) : HMap

  override def toString = s"HMap(${map.mkString(",")})"
}

object HMap {

  private class HMapI(val map: Map[HKey[_], Any]) extends HMap {
    def updated[T](k: HKey[T], v: T): HMap = new HMapI(map.updated(k, v))
    def ++( hmap: HMap ) = new HMapI( map ++ hmap.asInstanceOf[HMapI].map )
    def deleted[T](k: HKey[T]): HMap = new HMapI( map - k )
  }

  def apply(): HMap = new HMapI(Map())
}


object HMapSample extends App {

  object Keys {

    object importance extends HKey[Int]

    object name extends HKey[String]

    object address extends HKey[String]

    object dob extends HKey[Date]

    object emails extends HKey[List[String]]

  }

  import Keys._

  implicit val hmap = HMap()

  val plantilla = HMap().updated(importance,1)




  val hmap2 = (address <-- "a")(dob <-- new Date)(name <-- "pepe" )(emails <-- "a@a.com" :: "b@b.com" :: Nil )(plantilla)
  val hmap3 = (address <-- "b") ++ (dob <-- new Date) ++ (name <-- "juan" )

  println(hmap)
  println(hmap2)
  println(hmap3)

  println(hmap(name))
  println(hmap(address))
  println(address(hmap2))
  println(address())
  println(dob())

}
