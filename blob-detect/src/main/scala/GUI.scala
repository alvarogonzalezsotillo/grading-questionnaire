import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{SwingConstants, JTabbedPane, JComponent, JFrame}

import org.opencv.core.Mat

/**
 * Created by alvaro on 2/11/15.
 */
object GUI extends App{


  trait ProcessingStep{
    val processMat : (Mat) => Mat
    val stepName : String
  }

  case class Step(override val stepName :String,override val processMat:(Mat)=>Mat) extends ProcessingStep


  lazy val videoSource = new SwingVideoSource(VideoSource())(imageRead)

  var currentProcessingStep : Option[ProcessingStep] = null

  def imageRead(m : Mat) = currentProcessingStep.map(_.processMat(m))


  def createStepsComponent( steps: ProcessingStep* ) : JComponent = {

    class CanvasProcessingStep(s:ProcessingStep) extends ImageCanvas with ProcessingStep{
      val stepName = s.stepName
      val processMat = { (m:Mat) =>
        import Implicits._
        val ret = s.processMat(m)
        image = ret
        ret
      }
    }


    val ret = new JTabbedPane()
    ret.setTabPlacement(SwingConstants.RIGHT)

    def stepComponent( step: ProcessingStep ) = {
      val ps = new CanvasProcessingStep(step)
      ret.addTab(ps.stepName,ps)
    }

    ret.addChangeListener( new ChangeListener(){
      override def stateChanged(e: ChangeEvent) = {
        currentProcessingStep = Some(ret.getSelectedComponent.asInstanceOf[ProcessingStep])
      }
    })

    steps.foreach(stepComponent)

    ret

  }


  val frame = new JFrame("Corrección de exámenes")
  frame.add( createStepsComponent(
    Step( "Video original", m => m ),
    Step( "Umbral adaptativo", ImageProcessing.threshold() )
  ))

  frame.setSize(640,480)
  frame.setVisible(true)

}
