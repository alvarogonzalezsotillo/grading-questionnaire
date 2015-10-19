import java.awt.image.{DataBufferByte, BufferedImage}

import org.opencv.core.Mat

/**
 * Created by alvaro on 19/10/15.
 */
object Implicits {

  implicit def Mat2BufferedImage(m:Mat): BufferedImage = {
    // source: http://answers.opencv.org/question/10344/opencv-java-load-image-to-gui/
    // Fastest code
    // The output can be assigned either to a BufferedImage or to an Image

    val imageType = if ( m.channels() > 1 )
      BufferedImage.TYPE_3BYTE_BGR
    else
      BufferedImage.TYPE_BYTE_GRAY

    val bufferSize = m.channels()*m.cols()*m.rows()
    val b = new Array[Byte](bufferSize)
    m.get(0,0,b); // get all the pixels
    val image = new BufferedImage(m.cols(),m.rows(), imageType )
    val targetPixels = (image.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
    System.arraycopy(b, 0, targetPixels, 0, b.length)

    image
  }

  /*
  implicit def BufferedImage2Mat( image: BufferedImage ) : Mat = {
    val pixels = (image.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
    val ret = new Mat()
    ret.put(0,0,pixels)
    ret
  }
  */

}
