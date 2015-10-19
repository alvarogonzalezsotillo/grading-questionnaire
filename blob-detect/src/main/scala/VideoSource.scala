import org.opencv.core.Mat
import org.opencv.highgui.{Highgui, VideoCapture}

/**
 * Created by alvaro on 19/10/15.
 */


trait VideoSource{
  val camera : Int
  def lastRead : Mat
  def read : Mat
}


object VideoSource{
  def apply( cam: Int = 0 ) = new VideoSource{

    val camera = cam
    val vc = new VideoCapture()
    vc.open(0)
    if( !vc.isOpened ){
      throw new IllegalStateException()
    }
    vc.set(Highgui.CV_CAP_PROP_FRAME_WIDTH,1280)
    vc.set(Highgui.CV_CAP_PROP_FRAME_HEIGHT,1024)


    println( vc.get(Highgui.CV_CAP_PROP_FRAME_WIDTH) + "," + vc.get(Highgui.CV_CAP_PROP_FRAME_HEIGHT) )
    //println( vc.getSupportedPreviewSizes )


    val lastRead = new Mat()

    def read = synchronized{
      vc.read(lastRead)
      lastRead
    }
  }
}

