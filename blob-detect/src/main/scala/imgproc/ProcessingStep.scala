package imgproc


import org.opencv.core.{Mat, MatOfPoint, Scalar}

/**
 * Created by alvaro on 13/11/15.
 */


import imgproc.ProcessingStep._

trait ProcessingStep[SRC,DST] {
  val process: Process[SRC,DST]
  val stepName: String

  def extend[T](name: String)(p: Process[DST,T]) : ProcessingStep[SRC,T] = ExtendedStep(this,name,p)
}

case class Step[SRC,DST]( override val stepName: String)( override val process: Process[SRC,DST] ) extends ProcessingStep[SRC,DST]

case class ExtendedStep[SRC,DST,T]( previous: ProcessingStep[SRC,DST], override val stepName: String, extendedProcess: Process[DST,T] ) extends ProcessingStep[SRC,T]{
  override val process = (psi:ProcessingStepInfo[SRC]) => extendedProcess( previous.process(psi) )
}

case class InitialStep(override val stepName: String) extends ProcessingStep[Unit,Unit]{
  override val process = matrixOnlyProcess(m=>m)
}


object ProcessingStep{

  import imgproc.ImageProcessing._

  case class ProcessingStepInfo[T]( mat : Mat, info : T )

  type Process[SRC,DST] = (ProcessingStepInfo[SRC]) => ProcessingStepInfo[DST]

  implicit class UnitStep[T]( step: ProcessingStep[Unit,T] ){
    def processMat( m: Mat ) = step.process( ProcessingStepInfo(m,Unit) )
  }

  implicit class ContourStep[T]( step: ProcessingStep[T,Seq[MatOfPoint]] ) {
    def withDrawContours = {
      step.extend(step.stepName) { psi =>
        val ret = toColorImage( psi.mat )
        drawContours(ret, psi.info, new Scalar(255, 0, 255), 3)
        ProcessingStepInfo(ret, psi.info)
      }
    }
  }

  implicit class ContourOptionStep[T]( step: ProcessingStep[T,Option[MatOfPoint]] ) {
    def withDrawContour = {
      step.extend(step.stepName) { psi =>
        psi.info match {
          case Some(points) =>
            val ret = toColorImage(psi.mat)
            drawContours(ret, Seq(points), new Scalar(255, 0, 255), 3)
            ProcessingStepInfo(ret, psi.info)
          case None =>
            psi
        }
      }
    }
  }


  def matrixOnlyProcess( proc: (Mat)=>Mat ): Process[Unit,Unit] = {
    ( psi : ProcessingStepInfo[Unit] ) => ProcessingStepInfo( proc(psi.mat), Unit )
  }

  def contoursOnlyProcess( proc: Seq[MatOfPoint] => Seq[MatOfPoint] ) : Process[Seq[MatOfPoint],Seq[MatOfPoint]] = {
    ( psi : ProcessingStepInfo[Seq[MatOfPoint]] ) => ProcessingStepInfo( psi.mat, proc(psi.info) )
  }




  val initialStep = InitialStep( "Imagen original")

  val thresholdStep = initialStep.extend( "Umbral adaptativo")( matrixOnlyProcess( threshold() _ ) )

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)")( matrixOnlyProcess(clean()() _ ) )

  val contourStep = noiseReductionStep.extend("Búsqueda de contornos")( (psi : ProcessingStepInfo[Unit]) => ProcessingStepInfo( psi.mat, findContours(psi.mat) ) )
  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros")( contoursOnlyProcess( approximateContoursToQuadrilaterals() _ ) )

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros")( contoursOnlyProcess(findBiggestAlignedQuadrilaterals()  _ ) )

  val answerMatrixLocationStep: ProcessingStep[Unit, Option[MatOfPoint]] = biggestQuadrilateralsStep.extend( "Localización de la tabla de respuestas" ){ psi: ProcessingStepInfo[Seq[MatOfPoint]]  =>
    ProcessingStepInfo( psi.mat, locateAnswerMatrix()(psi.info) )
  }

  val answerMatrixStep = Step( "Extracción de la tabla de respuestas"){ psi: ProcessingStepInfo[Option[MatOfPoint]] =>
    psi.info match{
      case Some(rect) =>
        val h = findHomography(30)(rect)
        ProcessingStepInfo( warpImage()(psi.mat,h), Unit )

      case None =>
        ProcessingStepInfo(null,Unit )
    }
  }

}

