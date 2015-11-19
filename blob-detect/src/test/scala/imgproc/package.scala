import imgproc.Implicits.Epsilon

/**
 * Created by alvaro on 18/11/15.
 */
package object imgproc {

  implicit val epsilon = Epsilon(0.01)

}
