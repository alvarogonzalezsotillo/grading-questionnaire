package imgproc

import java.awt.Dimension

import com.github.sarxos.webcam.Webcam
import org.opencv.core.Mat
import org.opencv.highgui.{Highgui, VideoCapture}

/**
 * Created by alvaro on 19/10/15.
 */


trait VideoSource {
  val camera: Int

  def lastRead: Mat

  def read: Mat
}


object VideoSource {

  private class OpenCVVideoSource(override val camera: Int) extends VideoSource {

    val vc = new VideoCapture()
    vc.open(camera)
    if (!vc.isOpened) {
      throw new IllegalStateException()
    }
    vc.set(Highgui.CV_CAP_PROP_FRAME_WIDTH, 1280)
    vc.set(Highgui.CV_CAP_PROP_FRAME_HEIGHT, 1024)


    //println(vc.get(Highgui.CV_CAP_PROP_FRAME_WIDTH) + "," + vc.get(Highgui.CV_CAP_PROP_FRAME_HEIGHT))
    //println( vc.getSupportedPreviewSizes )


    val lastRead = new Mat()

    def read = synchronized {
      vc.read(lastRead)
      lastRead
    }
  }

  private class SarxosVideoSource(override val camera: Int) extends VideoSource {

    lazy val webcam = {
      val webcam = Webcam.getDefault
      val d = new Dimension(1280, 1024)
      webcam.setCustomViewSizes(Array[Dimension](d))
      webcam.setViewSize(d)
      //System.out.println("Camera size:" + webcam.getViewSize)
      webcam.open
      //System.out.println("Camera size:" + webcam.getViewSize)
      webcam
    }

    var _lastRead : Mat = new Mat
    var _someRead = false

    override def lastRead: Mat = _lastRead

    override def read: Mat = {
      import imgproc.Implicits._
      val image = webcam.getImage
      //System.out.println( image )
      //System.out.printf( s"${image.getWidth}x${image.getHeight}\n" )

      if( !_someRead ){
        //println( "Primera lectura")
        _lastRead = BufferedImage2Mat(image)
        //println( "convertido")
        _someRead = true
      }
      else {
        //println( "otras lecturas")
        BufferedImage2Mat(image,_lastRead)
      }
      _lastRead
    }
  }

  def apply(cam: Int = 0) : VideoSource = {
    //new OpenCVVideoSource(cam)
    new SarxosVideoSource(cam)
  }
}

