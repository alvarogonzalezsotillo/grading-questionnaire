import java.awt.{Image, Graphics, BorderLayout}
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing._

import org.opencv.core._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 2/11/15.
 */
object GUI extends App {


  trait ProcessingStep {
    val processMat: (Mat) => Mat
    val stepName: String
  }

  case class Step(override val stepName: String, override val processMat: (Mat) => Mat) extends ProcessingStep


  lazy val videoSource = new SwingVideoSource(VideoSource())(imageRead)

  var currentProcessingStep: Option[ProcessingStep] = null

  def imageRead(m: Mat) = currentProcessingStep.map(_.processMat(m))



  def createStepsComponent(steps: ProcessingStep*): JComponent = {

    class CanvasProcessingStep(s: ProcessingStep) extends ImageCanvas with ProcessingStep {
      val stepName = s.stepName

      private var overlay : Image = null


      val processMat = { (m: Mat) =>
        import Implicits._
        setOverlayImage(m)
        val ret = s.processMat(m)
        image = ret
        ret
      }

      private def setOverlayImage( m : Mat ) = {
        import Implicits._
        val mat  = new Mat
        Imgproc.pyrDown(m,mat)
        Imgproc.pyrDown(mat,mat)
        overlay = mat
      }

      override def paint( g: Graphics ) = {
        super.paint(g)
        g.drawImage(overlay,30,30,null)
      }

    }


    val ret = new JTabbedPane()
    ret.setTabPlacement(SwingConstants.RIGHT)

    def stepComponent(step: ProcessingStep) = {
      val ps = new CanvasProcessingStep(step)
      ret.addTab(ps.stepName, ps)
    }

    ret.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent) = {
        println("stateChanged")
        currentProcessingStep = Some(ret.getSelectedComponent.asInstanceOf[ProcessingStep])
        println(s"currentStep:$currentProcessingStep")
      }
    })

    steps.foreach(stepComponent)

    ret

  }


  nu.pattern.OpenCV.loadLibrary()

  import ImageProcessing._

  def detectContours(contoursFilter: Seq[MatOfPoint] => Seq[MatOfPoint] = identity)(m: Mat): Mat = {
    val cleaned = clean()()(threshold()(m))
    val contours = findContours(cleaned)

    drawContours(m, contoursFilter(contours), new Scalar(255, 0, 255), 3)
    m
  }

  def locateAnswerMatrixAsSeq( contours: Seq[MatOfPoint] ) = locateAnswerMatrix()(contours) match{
    case Some(contour) => Seq(contour)
    case None => Seq(new MatOfPoint(new Point(0,0) ) )
  }


  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())


  val frame = new JFrame("Corrección de exámenes")

  frame.add(createStepsComponent(
    Step("Video original", m => m),
    Step("Umbral adaptativo", threshold()),
    Step("Eliminación de ruido (open-close)", threshold() _ andThen clean()()),
    Step("Búsqueda de contornos", detectContours()),
    Step("Filtro de contronos no cuadriláteros", detectContours(approximateContoursToQuadrilaterals()) ),
    Step("Los mayores cinco cuadriláteros", detectContours( approximateContoursToQuadrilaterals() _ andThen findBiggestAlignedQuadrilaterals() ) ),
    Step("Tabla de respuestas", detectContours( approximateContoursToQuadrilaterals() _ andThen findBiggestAlignedQuadrilaterals() andThen locateAnswerMatrixAsSeq ) )
  ))

  frame.setSize(640, 480)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)

  videoSource.execute

}
