import java.awt.BorderLayout
import java.awt.image.{DataBufferByte, BufferedImage}
import java.util
import javax.swing.{ImageIcon, JLabel, JFrame}
import org.opencv.highgui.Highgui

import org.opencv.core._
import org.opencv.imgproc.Imgproc

object BlobDetect extends App{

  def loadLibrary() {
    nu.pattern.OpenCV.loadLibrary()
    //System.loadLibrary(org.opencv.core.Core.NATIVE_LIBRARY_NAME);
  }

  loadLibrary()




  import Implicits._

  def show( title: String, m : Mat ) = {
    val f = new JFrame(title)
    f.setLayout( new BorderLayout() )
    f.add( new ImageCanvas(m), BorderLayout.CENTER )
    f.pack()
    f.setVisible(true)
    f
  }

  def readAndShow( file: String ) = {
    val mat = Highgui.imread(file)
    show( file, mat )
  }



  readAndShow( getClass().getResource("2015-10-09-093027.jpg").getPath )
  show( "Hola" , VideoSource().read )

  def simpleTest() = {
    val m = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0))

    val mr1 = m.row(1)
    mr1.setTo(new Scalar(1))

    val mc5 = m.col(5)
    mc5.setTo(new Scalar(5))

    println(mc5)
  }



}