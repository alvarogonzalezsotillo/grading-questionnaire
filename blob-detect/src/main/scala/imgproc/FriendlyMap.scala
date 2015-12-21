package imgproc

/**
 * Created by alvaro on 21/12/15.
 */

trait FriendlyMap extends Dynamic{

}

object FriendlyMap {
  private class StepInfoImpl(parent: FriendlyMap) extends FriendlyMap{
    private val map = collection.mutable.Map[String,Any]()

    def selectDynamic[T](field: String) = {
      map.get(field)
    }

    def updateDynamic[T]( field: String )( value: T ): Unit ={
      map.put(field, value)
    }

    override def toString = map.toString

  }

  def apply : FriendlyMap = new StepInfoImpl(null)


}

object kk extends App{
  val fm = FriendlyMap

  println( fm )

  fm.nada = "de nada"

  println( fm )
}