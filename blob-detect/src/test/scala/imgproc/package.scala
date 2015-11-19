/**
 * Created by alvaro on 18/11/15.
 */
package object imgproc {
  case class Epsilon(epsilon:Double)

  implicit class DoubleComparator(val value:Double){
    def ~=(d:Double)(implicit epsilon: Epsilon) = Math.abs(this.value - d) < epsilon.epsilon
  }

  implicit val epsilon = Epsilon(0.01)
}
