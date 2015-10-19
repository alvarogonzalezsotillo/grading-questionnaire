import java.awt.{Color, BorderLayout}
import java.awt.image.{DataBufferByte, BufferedImage}
import java.util
import javax.swing.{JFrame, SwingWorker}

import org.opencv.core.Mat

/**
 * Created by alvaro on 19/10/15.
 */
object VideoCanvas{
  val defaultImage = new BufferedImage(10,10,BufferedImage.TYPE_INT_RGB)
  val g = defaultImage.getGraphics

  g.setColor(Color.blue)
  g.drawLine(0,0,10,10)
  g.dispose()
}



class VideoCanvas(camera: Int) extends ImageCanvas(VideoCanvas.defaultImage){
  val source = VideoSource(camera)

  var terminateASAP = false

  import Implicits._


  val worker = new SwingWorker[Unit,Mat]{
    override def doInBackground(): Unit = {
      while(!terminateASAP){
        val mat = source.read
        publish(mat)
      }

    }
    override def done() : Unit = {

    }
    override def process(chunks: util.List[Mat]): Unit = {
      val lastMat = chunks.get(chunks.size()-1)
      println( "Ya he read:" + lastMat.width() + "," + lastMat.height() )

      image = lastMat
    }
  }

  worker.execute()

  def stop = terminateASAP = true
}

object VideoCanvasApp extends App{

  nu.pattern.OpenCV.loadLibrary()

  val f = new JFrame("Video")
  f.setLayout( new BorderLayout() )
  f.add( new VideoCanvas(0), BorderLayout.CENTER )
  f.pack()
  f.setVisible(true)

}