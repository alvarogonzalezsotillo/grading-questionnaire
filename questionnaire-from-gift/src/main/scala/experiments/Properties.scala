package experiments

import experiments.Properties.{PropertyListener, DetachableListener}

import scala.collection.mutable

/**
 * Created by alvaro on 12/08/15.
 */
object Properties {

  object implicits {

    implicit def toValue[T](p: Prop[T]): T = p.get

  }


  case class DetachableListener[T](p: Prop[T], l: PropertyListener[T]) {
    def detach = p.removeListener(l)
  }

  type PropertyListener[T] = (Prop[T], T, T) => Unit


  trait Prop[T] extends (() => T) {
    def get: T

    private var listeners = Set[PropertyListener[T]]()

    protected def firePropertyChanged(oldV: T, newV: T) = listeners.foreach(l => l(this, oldV, newV))

    def addListener(l: PropertyListener[T]) : DetachableListener[T] = {
      listeners = listeners + l
      DetachableListener(this, l)
    }

    def addListener(l: => Unit ) : DetachableListener[T] = addListener( (_: Prop[T], _: T, _: T) => l )

    def removeListener(l: PropertyListener[T]) = listeners -= l

    def derive[U](f: (T) => U): Prop[U] = new DerivedProp(this, f)

    def apply = get
  }


  class RWProp[T](v: T) extends Prop[T] {
    private var value = v

    def get = value


    def set(v: T) = {
      if (value != v) {
        val oldValue = value
        value = v
        firePropertyChanged(oldValue, value)
      }
    }

    def update(v: T) = set(v)
  }

  object RWProp {
    def apply[T](v: T) = new RWProp(v)
  }

  private class DerivedProp[T, U](prop: Prop[U], f: (U) => T) extends Prop[T] {

    import implicits._

    private var value = f(prop)

    prop.addListener{ (_, oldValue, newValue) =>
      val oldValue = value
      value = f(newValue)
      firePropertyChanged(oldValue, value)
    }

    override def get = value
  }

}