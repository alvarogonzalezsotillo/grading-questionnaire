import java.awt.{GridLayout, Color, BorderLayout}
import java.awt.image.{DataBufferByte, BufferedImage}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.util
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JPanel, JSlider, JFrame, SwingWorker}

import org.opencv.core.{Size, CvType, Mat}
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 19/10/15.
 */
object VideoCanvas {
  val defaultImage = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
  val g = defaultImage.getGraphics

  g.setColor(Color.blue)
  g.drawLine(0, 0, 10, 10)
  g.dispose()
}


class VideoCanvas(camera: Int, proc: Option[(Mat) => Mat] = None) extends ImageCanvas(VideoCanvas.defaultImage) {
  val source = VideoSource(camera)

  var terminateASAP = false

  import Implicits._


  val worker = new SwingWorker[Unit, Mat] {
    override def doInBackground(): Unit = {
      while (!terminateASAP) {
        val mat = source.read
        val processed = proc match {
          case Some(p) => p(mat)
          case None => mat
        }
        publish(processed)
      }

    }

    override def done(): Unit = {

    }

    override def process(chunks: util.List[Mat]): Unit = {
      val lastMat = chunks.get(chunks.size() - 1)
      image = lastMat
    }
  }

  worker.execute()

  def stop = terminateASAP = true
}

object VideoCanvasApp extends App {

  import ImageProcessing._

  var sizeOpen = 18
  var sizeClose = 7



  nu.pattern.OpenCV.loadLibrary()

  val f = new JFrame("Video")
  f.setLayout(new BorderLayout())

  def proc(m: Mat) : Mat = clean(sizeOpen,sizeClose)( threshold()(m) )

  f.add(new VideoCanvas(0, Some(proc)), BorderLayout.CENTER)


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