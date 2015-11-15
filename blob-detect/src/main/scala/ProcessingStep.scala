
import org.opencv.core.{Scalar, MatOfPoint, Mat}

/**
 * Created by alvaro on 13/11/15.
 */



object ProcessingStep{

  import ImageProcessing._

  case class ProcessingStepInfo[T]( mat : Mat, info : T )

  trait ProcessingStep[SRC,DST] {
    val process: (ProcessingStepInfo[SRC]) => ProcessingStepInfo[DST]
    val stepName: String
  }

  case class Step[S,D](override val stepName: String, override val process: (ProcessingStepInfo[S]) => ProcessingStepInfo[D]) extends ProcessingStep[S,D]

  def withDrawContours[T]( s: Step[T,Seq[MatOfPoint]]) = {
    def drawContoursProcess( psi: ProcessingStepInfo[Seq[MatOfPoint]] ) = {
      val ret = psi.mat.clone()
      drawContours( ret, psi.info, new Scalar(255, 0, 255), 3 )
      ProcessingStepInfo( ret, psi.info )
    }

    Step( s.stepName, s.process andThen drawContoursProcess )
  }


  def matrixOnlyProcess( proc: (Mat)=>Mat ) = {
    ( psi : ProcessingStepInfo[Unit] ) => ProcessingStepInfo( proc(psi.mat), Unit )
  }

  val steps = Seq(
    Step("Video original",  matrixOnlyProcess(m => m) ),
    Step("Umbral adaptativo", matrixOnlyProcess( threshold() )),
    Step("Eliminación de ruido (open-close)", matrixOnlyProcess( clean()()) ),
    withDrawContours( Step("Búsqueda de contornos", (psi : ProcessingStepInfo[Unit]) => ProcessingStepInfo( psi.mat, findContours(psi.mat) ) ) )
      /*
    Step("Filtro de contronos no cuadriláteros", detectContours(approximateContoursToQuadrilaterals()) ),
    Step("Los mayores cinco cuadriláteros", detectContours( approximateContoursToQuadrilaterals() _ andThen findBiggestAlignedQuadrilaterals() ) ),
    Step("Tabla de respuestas", detectContours( approximateContoursToQuadrilaterals() _ andThen findBiggestAlignedQuadrilaterals() andThen locateAnswerMatrixAsSeq ) )
    */
  )

}

