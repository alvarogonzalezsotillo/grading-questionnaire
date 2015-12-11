package imgproc

import com.google.zxing.BinaryBitmap
import com.google.zxing.client.j2se.BufferedImageLuminanceSource
import com.google.zxing.common.HybridBinarizer
import com.google.zxing.qrcode.QRCodeReader
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.opencv.core.Mat

import scala.util.{Failure, Success, Try}

/**
 * Created by alvaro on 9/12/15.
 */
object QRScanner  extends LazyLogging{

  implicit def toBinaryBitmap(m:Mat) : BinaryBitmap = {
    import Implicits._

    val source = new BufferedImageLuminanceSource(m)
    new BinaryBitmap(new HybridBinarizer(source))
  }

  def decode( m: Mat ) : Option[String] = {
    val reader = new QRCodeReader()

    Try( reader.decode(m) ) match{
      case Success(result) =>
        Some(result.getText)
      case Failure(e) =>
        logger.error("Cant read QR", e)
        None
    }
  }
}
