package imgproc

import java.awt.{BorderLayout, GridLayout}
import java.util
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JFrame, JPanel, JSlider, SwingWorker}

import org.opencv.core.{Mat, Scalar}

/**
 * Created by alvaro on 19/10/15.
 */


class SwingVideoSource(source: VideoSource)( imageCaptured: (Mat) => Unit ) {

  var terminateASAP = false

  val worker = new SwingWorker[Unit, Mat] {
    override def doInBackground(): Unit = {
      while (!terminateASAP) {
        Thread.sleep(100)
        publish(source.read)
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
    import imgproc.Implicits._
    val processed : Mat = proc match{
      case Some(p) => p(m)
      case None => m
    }
    image = processed
  })


  swingSource.execute

  def stop = swingSource.stop
}

object VideoCanvasApp extends App {

  import imgproc.ImageProcessing._

  var sizeOpen = 18
  var sizeClose = 7
  var iterations = 1



  nu.pattern.OpenCV.loadLibrary()

  val f = new JFrame("Video")
  f.setLayout(new BorderLayout())


  def thresholdAndClean(m: Mat): Mat = clean(iterations,sizeOpen, sizeClose)()(threshold()(m))

  def detectContours(m: Mat): Mat = {
    val cleaned = clean(iterations,sizeOpen, sizeClose)()(threshold()(m)) //thresholdAndClean(m)
    val contours = findContours(cleaned)


    //drawContours(m,contours,new Scalar(0,255,0))
    drawContours(m, approximateContoursToQuadrilaterals(10)(contours), new Scalar(255, 0, 255), 3)
    m
  }

  f.add(new VideoCanvas(0, Some(thresholdAndClean)), BorderLayout.CENTER)


  val openSlider = new JSlider(1, 30)
  openSlider.setValue(sizeOpen)
  openSlider.getModel.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      sizeOpen = openSlider.getValue
      println( s"iterations:$iterations sizeOpen:$sizeOpen sizeClose:$sizeClose ")
    }
  })
  val closeSlider = new JSlider(1, 30)
  closeSlider.setValue(sizeClose)
  closeSlider.getModel.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      sizeClose = closeSlider.getValue
      println( s"iterations:$iterations sizeOpen:$sizeOpen sizeClose:$sizeClose ")
    }
  })
  val iterationsSlider = new JSlider(1,20)
  iterationsSlider.setValue(1)
  iterationsSlider.getModel.addChangeListener( new ChangeListener {

    override def stateChanged(e: ChangeEvent): Unit = {
      iterations = iterationsSlider.getValue
      println( s"iterations:$iterations sizeOpen:$sizeOpen sizeClose:$sizeClose ")
    }
  })

  val sliders = new JPanel(new GridLayout(3, 1))
  sliders.add(openSlider)
  sliders.add(closeSlider)
  sliders.add(iterationsSlider)
  f.add(sliders, BorderLayout.NORTH)


  f.pack()
  f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  f.setVisible(true)

}