package imgproc

import java.awt.{Graphics, Image}
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

import org.opencv.core._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 2/11/15.
 */
object GUI extends App {



  private var currentProcessingStep: Option[CanvasProcessingStep[_]] = null

  private def imageRead(m: Mat) = currentProcessingStep.map(_.processMat(m))

  private lazy val videoSource = new SwingVideoSource(VideoSource())(imageRead)



  class CanvasProcessingStep[T](val step: ProcessingStep[Unit,T]) extends ImageCanvas{

    private var overlay : Image = null


    def processMat(m: Mat) = {
      import imgproc.Implicits._
      setOverlayImage(m)
      val ret = step.processMat(m)
      image = ret.mat
      ret
    }

    private def setOverlayImage( m : Mat ) = {
      import imgproc.Implicits._
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

  private def createStepsComponent(steps: ProcessingStep[Unit,_]*): JComponent = {

    val ret = new JTabbedPane()
    ret.setTabPlacement(SwingConstants.LEFT)

    def stepComponent(step: ProcessingStep[Unit,_]) = {
      val ps = new CanvasProcessingStep(step)
      ret.addTab(ps.step.stepName, ps)
    }

    ret.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent) = {
        println("stateChanged")
        currentProcessingStep = Some(ret.getSelectedComponent.asInstanceOf[CanvasProcessingStep[_]])
        println(s"currentStep:$currentProcessingStep")
      }
    })

    steps.foreach(stepComponent)

    ret

  }


  nu.pattern.OpenCV.loadLibrary()

  import imgproc.ProcessingStep._

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())


  private val frame = new JFrame("Corrección de exámenes")

  frame.add(createStepsComponent(
    initialStep,
    thresholdStep,
    noiseReductionStep,
    contourStep.withDrawContours,
    quadrilateralStep.withDrawContours,
    biggestQuadrilateralsStep.withDrawContours,
    answerMatrixLocationStep.withDrawContour,
    answerMatrixStep(),
    cellsOfAnswerMatrix().withDrawContours,
    cellsOfAnswerMatrix().withDrawContours.withSaveMatrix()
  ))

  frame.setSize(640, 480)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)

  videoSource.execute

}
