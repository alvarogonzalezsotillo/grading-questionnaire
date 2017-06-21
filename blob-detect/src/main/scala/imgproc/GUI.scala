package imgproc

import java.awt.{Graphics, Image}
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

import common.Logging
import imgproc.steps.AnswersInfo.{cells, cellsLocation}
import imgproc.steps.ContoursInfo.{biggestQuadrilaterals, contours, quadrilaterals}
import imgproc.steps.LocationInfo.location
import imgproc.steps.MainInfo.mat
import imgproc.steps.{AnswersInfo, LocationInfo, ProcessingStep}
import imgproc.steps.QRInfo.{qrLocation, qrText}
import org.opencv.core._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 2/11/15.
 */
object GUI extends App {



  private var currentProcessingStep: Option[CanvasProcessingStep] = null

  private def imageRead(m: Mat) = currentProcessingStep.map(_.processMat(m))

  private lazy val videoSource = new SwingVideoSource(VideoSource())(imageRead)



  class CanvasProcessingStep(val step: ProcessingStep) extends ImageCanvas{

    private var overlay : Image = null


    def processMat(m: Mat) = {
      import imgproc.Implicits._
      import imgproc.steps.MainInfo._
      import imgproc.steps.ProcessingStep.Implicits._

      setOverlayImage(m)
      val ret = step.process(m)
      image = ret(mat).getOrElse(null)
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

  private def createStepsComponent(steps: ProcessingStep*): JComponent = {

    val ret = new JTabbedPane()
    ret.setTabPlacement(SwingConstants.LEFT)

    def stepComponent(step: ProcessingStep) = {
      val ps = new CanvasProcessingStep(step)
      ret.addTab(ps.step.stepName, ps)
    }

    ret.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent) = {
        println("stateChanged")
        currentProcessingStep = Some(ret.getSelectedComponent.asInstanceOf[CanvasProcessingStep])
        println(s"currentStep:$currentProcessingStep")
      }
    })

    steps.foreach(stepComponent)

    ret

  }


  nu.pattern.OpenCV.loadLibrary()

  import ProcessingStep._

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())


  private val frame = new JFrame("Corrección de exámenes")

  frame.add(createStepsComponent(
    initialStep,
    thresholdStep,
    noiseReductionStep,
    contourStep.withDrawContours(_(contours)),
    quadrilateralStep.withDrawContours(_(quadrilaterals)),
    biggestQuadrilateralsStep.withDrawContours(_(biggestQuadrilaterals)),
    locateQRStep.withDrawContours( i=> i(qrLocation).map( c => Seq(c) )),
    extractQRStep,
    decodeQRStep.withDrawString( _(qrText) ),
    cellsLocationStep.withDrawContours( _(cellsLocation) ),
    studentAnswersStep,
    cellsLocationStep.withFilter()(_(cellsLocation).isDefined).withSaveMatrix(LocationInfo.locatedMat)
  ))

  frame.setSize(640, 480)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)

  Logging.disableLogging()
  videoSource.execute


}
