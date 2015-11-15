
import org.opencv.core.{Scalar, MatOfPoint, Mat}

/**
 * Created by alvaro on 13/11/15.
 */



object ProcessingStep{

  import ImageProcessing._

  case class ProcessingStepInfo[T]( mat : Mat, info : T )

  type Process[SRC,DST] = (ProcessingStepInfo[SRC]) => ProcessingStepInfo[DST]

  trait ProcessingStep[SRC,DST] {
    val process: Process[SRC,DST]
    val stepName: String

    def extend[T](name: String)(p: Process[DST,T]) = ExtendedStep(this,name,p)
  }

  case class ExtendedStep[SRC,DST,T]( previous: ProcessingStep[SRC,DST], override val stepName: String, extendedProcess: Process[DST,T] ) extends ProcessingStep[SRC,T]{
    override val process = (psi:ProcessingStepInfo[SRC]) => extendedProcess( previous.process(psi) )
  }

  case class InitialStep(override val stepName: String) extends ProcessingStep[Unit,Unit]{
    override val process = matrixOnlyProcess(m=>m)
  }

  def withDrawContours[T]( s: ProcessingStep[T,Seq[MatOfPoint]]) = {
    s.extend( s.stepName){ psi =>
      val ret = psi.mat.clone()
      drawContours( ret, psi.info, new Scalar(255, 0, 255), 3 )
      ProcessingStepInfo( ret, psi.info )
    }
  }

  implicit def matrixOnlyProcess( proc: (Mat)=>Mat ): (ProcessingStepInfo[Unit]) => ProcessingStepInfo[Unit] = {
    ( psi : ProcessingStepInfo[Unit] ) => ProcessingStepInfo( proc(psi.mat), Unit )
  }

  implicit def contoursOnlyProcess( proc: Seq[MatOfPoint] => Seq[MatOfPoint] ) : (ProcessingStepInfo[Seq[MatOfPoint]]) => ProcessingStepInfo[Seq[MatOfPoint]] = {
    ( psi : ProcessingStepInfo[Seq[MatOfPoint]] ) => ProcessingStepInfo( psi.mat, proc(psi.info) )
  }

  val steps : Pair[String,Process[_,_]] = Seq(
    ("Umbral adaptativo", threshold() _ ),
    ("Eliminación de ruido (open-close)", clean()() _ ),
    ("Búsqueda de contornos", (psi : ProcessingStepInfo[Unit]) => ProcessingStepInfo( psi.mat, findContours(psi.mat) ) ),
    ("Filtro de contronos no cuadriláteros", approximateContoursToQuadrilaterals() _ ),
    ("Los mayores cinco cuadriláteros", findBiggestAlignedQuadrilaterals()  _ ),
    ("Tabla de respuestas", (psi: ProcessingStepInfo[Seq[MatOfPoint]] ) => ProcessingStepInfo( psi.mat, locateAnswerMatrix(psi.info) ) )

  )

}

