import java.awt.AlphaComposite
import java.awt.image.{DataBufferByte, BufferedImage}

import org.opencv.core.{CvType, Mat}

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


  implicit def BufferedImage2Mat( image: BufferedImage ) : Mat = {
    val ret = new Mat(image.getHeight,image.getWidth,CvType.CV_8UC3)
    BufferedImage2Mat(image, ret)
    ret
  }


  def toBufferedImageOfType(original:BufferedImage, typeI: Int) = {

    // Don't convert if it already has correct type
    if (original.getType() == typeI) {
      original
    }
    else {
      // Create a buffered image
      val image = new BufferedImage(original.getWidth(), original.getHeight(), typeI)

      // Draw the image onto the new buffer
      val g = image.createGraphics();
      try {
        g.setComposite(AlphaComposite.Src);
        g.drawImage(original, 0, 0, null);
      }
      finally {
        g.dispose()
      }

      image
    }
  }
  def BufferedImage2Mat( src: BufferedImage, dst: Mat ){
    val image = toBufferedImageOfType(src,BufferedImage.TYPE_3BYTE_BGR)
    try {
      //println(Thread.currentThread().getName)
      //println("a")
      assert(image.getType == BufferedImage.TYPE_3BYTE_BGR)
      //println("b")
      assert(dst.`type` == CvType.CV_8UC3)
      //println(dst.width() + " " + image.getWidth )
      assert(dst.width == image.getWidth)
      //println("d")
      assert(dst.height == image.getHeight)
      //println("e")
      val pixels = (image.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
      //println("f")
      dst.put(0, 0, pixels)
      //println("g")
    }
    catch{
      case t : Throwable => t.printStackTrace()
    }
  }


}
