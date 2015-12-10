package imgproc


import java.util.Date

import org.opencv.core._

import org.opencv.imgproc.Imgproc



/**
 * Created by alvaro on 13/11/15.
 */


import imgproc.ProcessingStep._

trait ProcessingStep[SRC,DST] {
  val process: Process[SRC,DST]
  val stepName: String

  def extend[T](name: String)(p: Process[DST,T]) : ProcessingStep[SRC,T] = ExtendedStep(this,name,p)
}



object ProcessingStep{

  import imgproc.Implicits._

  import imgproc.ImageProcessing._

  case class ProcessingStepInfo[T]( mat : Mat, info : T )

  type Process[SRC,DST] = (ProcessingStepInfo[SRC]) => ProcessingStepInfo[DST]

  private case class Step[SRC,DST]( override val stepName: String)( override val process: Process[SRC,DST] ) extends ProcessingStep[SRC,DST]

  private case class ExtendedStep[SRC,DST,T]( previous: ProcessingStep[SRC,DST], override val stepName: String, extendedProcess: Process[DST,T] ) extends ProcessingStep[SRC,T]{
    override val process = (psi:ProcessingStepInfo[SRC]) => extendedProcess( previous.process(psi) )
  }

  private case class InitialStep(override val stepName: String) extends ProcessingStep[Unit,Unit]{
    override val process = matrixOnlyProcess(m=>m)
  }


  implicit class UnitStep[T]( step: ProcessingStep[Unit,T] ){
    def processMat( m: Mat ) = step.process( ProcessingStepInfo(m,Unit) )
  }

  implicit class ContourStep[T]( step: ProcessingStep[T,Seq[MatOfPoint]] ) {
    def withDrawContours = {
      step.extend(step.stepName) { psi =>
        if( psi.mat != null ) {
          val ret = toColorImage(psi.mat)
          drawContours(ret, psi.info, new Scalar(255, 0, 255) )
          ProcessingStepInfo(ret, psi.info)
        }
        else{
          psi
        }
      }
    }
  }

  implicit class ContourOptionStep[T]( step: ProcessingStep[T,Option[MatOfPoint]] ) {
    def withDrawContour = {
      step.extend(step.stepName) { psi =>
        psi.info match {
          case Some(points) =>
            val ret = toColorImage(psi.mat)
            drawContours(ret, Seq(points), new Scalar(255, 0, 255) )
            ProcessingStepInfo(ret, psi.info)
          case None =>
            psi
        }
      }
    }
  }
    
  implicit class MatrixStep[SRC,DST]( step: ProcessingStep[SRC,DST] ){

      private val delay = 2500

      private def defaultAccept( psi: ProcessingStepInfo[DST]) = {
        psi.mat != null
      }

      private var lastSave = System.currentTimeMillis()

      def withSaveMatrix( accept: ProcessingStepInfo[DST] => Boolean = defaultAccept _ ) = {
          
         step.extend( "Guardar imagen de: " + step.stepName ){ psi: ProcessingStepInfo[DST] =>
        
           if( accept(psi) && System.currentTimeMillis() > lastSave + delay ){
               lastSave = System.currentTimeMillis()
               save(psi.mat)
           }
           psi
        }
      }
      
  
    
    private def save( m: Mat ){
        val shortDateFormat = new java.text.SimpleDateFormat("yyyyMMdd")
        val longDateFormat = new java.text.SimpleDateFormat("yyyyMMdd-HHmmss")
        val date = new java.util.Date
        val ldate = longDateFormat.format(date)
        val sdate = shortDateFormat.format(date)
        new java.io.File(sdate).mkdirs
        val file = s"$sdate/$ldate.png"
        org.opencv.highgui.Highgui.imwrite(file,m)

    } 
  }
      
  

  private def matrixOnlyProcess( proc: (Mat)=>Mat ): Process[Unit,Unit] = {
    ( psi : ProcessingStepInfo[Unit] ) => ProcessingStepInfo( proc(psi.mat), Unit )
  }

  private def contoursOnlyProcess( proc: Seq[MatOfPoint] => Seq[MatOfPoint] ) : Process[Seq[MatOfPoint],Seq[MatOfPoint]] = {
    ( psi : ProcessingStepInfo[Seq[MatOfPoint]] ) => ProcessingStepInfo( psi.mat, proc(psi.info) )
  }

  def recoverOriginalMatrixStep[SRC,DST]( step : ProcessingStep[SRC,DST] ) : ProcessingStep[SRC,DST] = Step( step.stepName ){ psi: ProcessingStepInfo[SRC] =>
    ProcessingStepInfo(psi.mat, step.process(psi).info )
  }


  def locateQR(answerMatrixLocation: MatOfPoint): MatOfPoint = {
    val points = answerMatrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val xaxis = (tr - tl)
    val yaxis = new Point(-xaxis.y, xaxis.x)

    val topLeft = tl - (yaxis * AnswerMatrixMeasures.matrixWithToTopOfQRRatio)
    val topRight = topLeft + (xaxis*AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomLeft = topLeft + (yaxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomRight = topRight + (yaxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)

    new MatOfPoint(topLeft,topRight,bottomRight,bottomLeft)
  }


  def locateAnswerMatrix(number: Int = 5, insideLimit:Int = -8)(contours: Seq[MatOfPoint]): Option[MatOfPoint] = {
    import scala.collection.JavaConverters._

    val allPoints = contours.map(_.toList.asScala).flatten


    def doIt() = {
      val (center, orientation) = {
        val shapes = contours.map(c => new Shape(c))
        val leftmostCenter = shapes.map(_.center).minBy(_.x)
        val rightmostCenter = shapes.map(_.center).maxBy(_.x)

        ((leftmostCenter + rightmostCenter) * 0.5, (rightmostCenter - leftmostCenter))
      }

      val difs = allPoints.map(_ - center)

      val unit = orientation.normalize

      val (upperLeft, upperRight) = {
        val upperPoints = difs.filter(_.crossProductZ(unit) > 0)
        (upperPoints.minBy(_.normalize * unit), upperPoints.maxBy(_.normalize * unit))
      }

      val (lowerLeft, lowerRight) = {
        val lowerPoints = difs.filter(_.crossProductZ(unit) < 0)
        (lowerPoints.minBy(_.normalize * unit), lowerPoints.maxBy(_.normalize * unit))
      }

      val lowerExtension = (lowerRight - lowerLeft) * AnswerMatrixMeasures.extensionFactor
      val upperExtension = (upperRight - upperLeft) * AnswerMatrixMeasures.extensionFactor

      new MatOfPoint(
        upperLeft + center,
        upperRight + center + upperExtension,
        lowerRight + center + lowerExtension,
        lowerLeft + center
      )
    }

    def checkIt(contour: MatOfPoint) = {
      contours.size == number &&
        allPoints.forall { p =>
          val contour2f = new MatOfPoint2f()
          contour.convertTo(contour2f, CvType.CV_32FC2)
          val inside = Imgproc.pointPolygonTest(contour2f, p, true)
          inside > insideLimit
        }
    }

    doIt If checkIt _
  }


  val initialStep : ProcessingStep[Unit,Unit] = InitialStep( "Imagen original")

  val thresholdStep = initialStep.extend( "Umbral adaptativo")( matrixOnlyProcess( threshold() _ ) )

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)")( matrixOnlyProcess(clean()() _ ) )

  val contourStep = recoverOriginalMatrixStep( noiseReductionStep.extend("Búsqueda de contornos")( (psi : ProcessingStepInfo[Unit]) => ProcessingStepInfo( psi.mat, findContours(psi.mat) ) ) )

  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros")( contoursOnlyProcess( approximateContoursToQuadrilaterals() _ ) )

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros")( contoursOnlyProcess(findBiggestAlignedQuadrilaterals()  _ ) )

  val answerMatrixLocationStep: ProcessingStep[Unit, Option[MatOfPoint]] = biggestQuadrilateralsStep.extend( "Localización de la tabla de respuestas" ){ psi: ProcessingStepInfo[Seq[MatOfPoint]]  =>
    ProcessingStepInfo( psi.mat, locateAnswerMatrix()(psi.info) )
  }


  val locateQRStep = answerMatrixLocationStep.extend( "Localización del código QR" ){ psi: ProcessingStepInfo[Option[MatOfPoint]]  =>
    ProcessingStepInfo( psi.mat, psi.info.map(locateQR) )
  }


  private val defaultQuestions = 50

  def answerMatrixStep( questions: Int = defaultQuestions ): ProcessingStep[Unit, Unit] = {
    answerMatrixLocationStep.extend( "Extracción de la tabla de respuestas"){ psi: ProcessingStepInfo[Option[MatOfPoint]] =>
      psi.info match{
        case Some(rect) =>

          val h = findHomography(questions)(rect)
          ProcessingStepInfo( warpImage()(psi.mat,h,AnswerMatrixMeasures.destinationSize(questions)), Unit)

        case None =>
          ProcessingStepInfo(null,Unit)
      }
    }
  }

  def cellsOfAnswerMatrix( questions: Int = defaultQuestions ) = {
    answerMatrixStep(questions).extend( "Localización de celdas"){ psi: ProcessingStepInfo[Unit] =>
      ProcessingStepInfo(psi.mat,AnswerMatrixMeasures.cells(questions))
    }
  }

}

