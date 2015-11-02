import java.awt.{GridLayout, Color, BorderLayout}
import java.awt.image.{DataBufferByte, BufferedImage}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.util
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JPanel, JSlider, JFrame, SwingWorker}

import org.opencv.core.{Scalar, Size, CvType, Mat}
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 19/10/15.
 */


class SwingVideoSource(source: VideoSource)( imageCaptured: (Mat) => Unit ) {

  var terminateASAP = false

  val worker = new SwingWorker[Unit, Mat] {
    override def doInBackground(): Unit = {
      while (!terminateASAP) {
        publish(source.read)
        println( "source.read" )
      }

    }

    override def done(): Unit = {

    }

    override def process(chunks: util.List[Mat]): Unit = {
      val lastMat = chunks.get(chunks.size() - 1)
      imageCaptured(lastMat)
    }


  }

  def execute = worker.execute()

  def stop = terminateASAP = true
}

class VideoCanvas(camera: Int, proc: Option[(Mat) => Mat] = None) extends ImageCanvas {
  val source = VideoSource(camera)
  val swingSource = new SwingVideoSource(source)({ m : Mat =>
    import Implicits._
    val processed : Mat = proc match{
      case Some(p) => p(m)
      case None => m
    }
    image = processed
  })




  def stop = swingSource.stop
}

object VideoCanvasApp extends App {

  import ImageProcessing._

  var sizeOpen = 18
  var sizeClose = 7


  nu.pattern.OpenCV.loadLibrary()

  val f = new JFrame("Video")
  f.setLayout(new BorderLayout())


  def thresholdAndClean(m: Mat): Mat = clean(sizeOpen, sizeClose)()(threshold()(m))

  def detectContours(m: Mat): Mat = {
    val cleaned = clean(sizeOpen, sizeClose)()(threshold()(m)) //thresholdAndClean(m)
    val contours = findContours(cleaned)


    //drawContours(m,contours,new Scalar(0,255,0))
    drawContours(m, approximateContoursToQuadrilaterals(10)(contours), new Scalar(255, 0, 255), 3)
    m
  }

  f.add(new VideoCanvas(0, Some(detectContours)), BorderLayout.CENTER)


  val openSlider = new JSlider(1, 30)
  openSlider.setValue(sizeOpen)
  openSlider.getModel.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      sizeOpen = openSlider.getValue
      println(s"sizeOpen:$sizeOpen")
    }
  })
  val closeSlider = new JSlider(1, 30)
  closeSlider.setValue(sizeClose)
  closeSlider.getModel.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      sizeClose = closeSlider.getValue
      println(s"sizeClose:$sizeClose")
    }
  })

  val sliders = new JPanel(new GridLayout(2, 1))
  sliders.add(openSlider)
  sliders.add(closeSlider)
  f.add(sliders, BorderLayout.NORTH)


  f.pack()
  f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  f.setVisible(true)

}