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

  val mLowerBound = new Scalar(0);
  val mUpperBound = new Scalar(0);

  val mPyrDownMat = new Mat()
  val mHsvMat = new Mat()
  val mMask = new Mat()
  val mDilatedMask = new Mat()
  val mHierarchy = new Mat()
  val mMinContourArea = 0.1


  def process(rgbaImage: Mat) = {
    import scala.collection.JavaConversions._

    Imgproc.pyrDown(rgbaImage, mPyrDownMat);
    Imgproc.pyrDown(mPyrDownMat, mPyrDownMat);

    Imgproc.cvtColor(mPyrDownMat, mHsvMat, Imgproc.COLOR_RGB2HSV_FULL);

    Core.inRange(mHsvMat, mLowerBound, mUpperBound, mMask);
    Imgproc.dilate(mMask, mDilatedMask, new Mat());

    val contours : java.util.List[MatOfPoint] = new util.ArrayList[MatOfPoint]();

    Imgproc.findContours(mDilatedMask, contours, mHierarchy, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);

    // Find max contour area
    val maxArea = contours.map( Imgproc.contourArea ).max

    // Filter contours by area and resize to fit the original image size
    contours.filter( c => Imgproc.contourArea(c) > mMinContourArea*maxArea ).map( c => Core.multiply(c, new Scalar(4,4), c ) )

  }

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

  def simpleTest() = {
    val m = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0))

    val mr1 = m.row(1)
    mr1.setTo(new Scalar(1))

    val mc5 = m.col(5)
    mc5.setTo(new Scalar(5))

    println(mc5)
  }



}