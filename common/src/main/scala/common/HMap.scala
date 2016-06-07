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
}

sealed trait HMap {

  def keys : Iterable[HKey[_]]

  def apply[T](key: HKey[T]): Option[T]

  def updated[T](k: HKey[T], v: T): HMap

  def deleted[T](k: HKey[T]): HMap

  def updated[T](k: HKey[T], ov: Option[T] ) : HMap = ov match{
    case Some(v) => updated(k,v)
    case None => deleted(k)
  }

  def apply[T]( k: HKey[T], v: T ) = updated(k,v)
  def apply[T]( k: HKey[T], v: Option[T] ) = updated(k,v)

  def ++( hmap: HMap ) : HMap

}

object HMap {

  private class HMapI(val map: Map[HKey[_], Any]) extends HMap {
    def apply[T](key: HKey[T]): Option[T] = map.get(key).asInstanceOf[Option[T]]

    def updated[T](k: HKey[T], v: T): HMap = new HMapI(map.updated(k, v))
    def ++( hmap: HMap ) = new HMapI( map ++ hmap.asInstanceOf[HMapI].map )
    def deleted[T](k: HKey[T]): HMap = new HMapI( map - k )

    override def toString = s"HMap(${map.mkString(",")})"

    override def keys = map.keys
  }

  def apply(): HMap = new HMapI(Map())
}

