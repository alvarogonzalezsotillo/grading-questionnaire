package imgproc


import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import org.opencv.core._
import org.opencv.highgui.Highgui

import org.opencv.imgproc.Imgproc


/**
 * Created by alvaro on 13/11/15.
 */


trait ProcessingStep[SRC, DST] {

  import imgproc.ProcessingStep._

  val process: SRC => DST
  val stepName: String

  def extend[T](name: String)(p: DST => T): ProcessingStep[SRC, T] = ExtendedStep(this, name, p)

}


object ProcessingStep {

  import imgproc.Implicits._

  import imgproc.ImageProcessing._



  trait MatInfo{
    val mat: Mat
  }

  trait OptionContourInfo{
    val optionContour : Option[MatOfPoint]
  }

  trait ContoursInfo{
    val contours : Seq[MatOfPoint]
  }

  trait StringInfo{
    val msg : Option[String]
  }

  def matrixOnlyProcess[SRC <: MatInfo]( f: Mat => Mat ) = {
    (info: SRC) => new MatInfo{
      val mat = f(info.mat)
    }
  }



  type StepFilter[T] = (T) => Boolean

  private case class Step[SRC, DST](override val stepName: String)(override val process: SRC => DST) extends ProcessingStep[SRC, DST]

  private case class ExtendedStep[SRC, DST, T](previous: ProcessingStep[SRC, DST], override val stepName: String, extendedProcess: DST => T) extends ProcessingStep[SRC, T] {
    override val process = (src:SRC) => extendedProcess(previous.process(src))
  }

  private case class InitialStep[T](override val stepName: String) extends ProcessingStep[Unit, T] {
    override val process = (m:T) => m
  }


  implicit class ContourStep[T,DST<: MatInfo with ContoursInfo](step: ProcessingStep[T, DST]) {
    def withDrawContours = {
      step.extend(step.stepName) { info =>
        if (info.mat != null) {
          val ret = toColorImage(info.mat)
          drawContours(ret, info.contours, new Scalar(255, 0, 255))
          new DST{
            val mat = ret
            val contours = info.contours
          }

        }
        else {
          info
        }
      }
    }
  }

  implicit class ContourOptionStep[T,DST<: MatInfo with OptionContourInfo](step: ProcessingStep[T, DST]) {
    def withDrawContour = {
      step.extend(step.stepName) { info =>
        info.optionContour match {
          case Some(points) =>
            val ret = toColorImage(info.mat)
            drawContours(ret, Seq(points), new Scalar(255, 0, 255))
            new DST{
              val mat = ret
              val optionContour = info.optionContour
            }
          case None =>
            info
        }
      }
    }
  }

  implicit class StringOptionStep[T,DST<: MatInfo with StringInfo]( step: ProcessingStep[T,DST]){
    def withDrawString = step.extend(step.stepName){ psi =>
      psi.msg match{
        case Some(s) =>
          val ret = psi.mat.clone
          drawString( ret, s, new Scalar(255, 0, 255), (1.0,5.0) )
          new DST{
            val mat = ret
            val msg = psi.msg
          }
        case None =>
          psi
      }

    }
  }

  implicit class MatrixStep[SRC, DST <: MatInfo](step: ProcessingStep[SRC, DST]) {

    private val defaultDelay = 2500

    private def defaultAccept(psi: DST) = psi.mat != null

    private def withFilterAndAction(name: String, delay: Int = defaultDelay, accept: DST => Boolean, action: DST => Unit) = {
      var lastAccept = System.currentTimeMillis()
      var lastInfo: DST = null
      step.extend(name) { psi =>
        if (accept(psi) && System.currentTimeMillis() > lastAccept + delay) {
          lastAccept = System.currentTimeMillis()
          val m = action( psi.mat.clone )

          new DST{
            val mat : lastMat
          }
        }
        ProcessingInfo(lastMat, psi.info)
      }
    }

    def withFilter(name: String = "Guardar imagen:" + step.stepName, delay: Int = defaultDelay)(accept: ProcessingInfo[DST] => Boolean = defaultAccept _) = {
      withFilterAndAction(name, delay, accept, psi => ())
    }

    def withSaveMatrix(name: String = "Guardar imagen:" + step.stepName, delay: Int = defaultDelay)(accept: ProcessingInfo[DST] => Boolean = defaultAccept _) = {

      def save(m: Mat) {
        val shortDateFormat = new SimpleDateFormat("yyyyMMdd")
        val longDateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
        val date = new Date
        val lDate = longDateFormat.format(date)
        val sDate = shortDateFormat.format(date)
        new File(sDate).mkdirs
        val file = s"$sDate/$lDate.png"
        Highgui.imwrite(file, m)
      }

      withFilterAndAction(name, delay, accept, psi => save(psi.mat))
    }


  }




  def locateQR(answerMatrixLocation: MatOfPoint): MatOfPoint = {
    val points = answerMatrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val xaxis = (tr - tl)
    val yaxis = new Point(-xaxis.y, xaxis.x)

    val topLeft = tl -
      (yaxis * AnswerMatrixMeasures.matrixWithToTopOfQRRatio) -
      (xaxis * AnswerMatrixMeasures.matrixWithToLeftOfQRRatio)
    val topRight = topLeft + (xaxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomLeft = topLeft + (yaxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomRight = topRight + (yaxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)

    new MatOfPoint(topLeft, topRight, bottomRight, bottomLeft)
  }


  def locateAnswerMatrix(number: Int = 5, insideLimit: Int = -8)(contours: Seq[MatOfPoint]): Option[MatOfPoint] = {
    import scala.collection.JavaConverters._

    val allPoints = contours.map(_.toList.asScala).flatten


    def doIt() = {
      val (center, orientation) = {
        val shapes = contours.map(c => new Shape(c))
        val leftmostCenter = shapes.map(_.center).minBy(_.x)
        val rightmostCenter = shapes.map(_.center).maxBy(_.x)

        ((leftmostCenter + rightmostCenter) * 0.5, (rightmostCenter - leftmostCenter))
      }

      val diffs = allPoints.map(_ - center)

      val unit = orientation.normalize

      val (upperLeft, upperRight) = {
        val upperPoints = diffs.filter(_.crossProductZ(unit) > 0)
        (upperPoints.minBy(_.normalize * unit), upperPoints.maxBy(_.normalize * unit))
      }

      val (lowerLeft, lowerRight) = {
        val lowerPoints = diffs.filter(_.crossProductZ(unit) < 0)
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


  val initialStep: ProcessingStep[Unit, MatInfo] = InitialStep("Imagen original")

  val thresholdStep = initialStep.extend("Umbral adaptativo"){
    matrixOnlyProcess(threshold()(_))
  }

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)"){
    matrixOnlyProcess(clean()() _)
  }

  def recoverOriginalMatrixStep(step: ProcessingStep[Unit, MatInfo with ContoursInfo) = {
    
  }

  val contourStep = recoverOriginalMatrixStep{
    noiseReductionStep.extend("Búsqueda de contornos"){
      (info: MatInfo) => new MatInfo with ContoursInfo{
        val mat = info.mat
        val contours = findContours(info.mat)
      }
    }
  }

  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros")(contoursOnlyProcess(approximateContoursToQuadrilaterals() _))

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros")(contoursOnlyProcess(findBiggestAlignedQuadrilaterals() _))

  val answerMatrixLocationStep: ProcessingStep[Unit, Option[MatOfPoint]] = biggestQuadrilateralsStep.extend("Localización de la tabla de respuestas") { psi: ProcessingInfo[Seq[MatOfPoint]] =>
    ProcessingInfo(psi.mat, locateAnswerMatrix()(psi.info))
  }


  val locateQRStep = answerMatrixLocationStep.extend("Localización del código QR") { psi: ProcessingInfo[Option[MatOfPoint]] =>
    ProcessingInfo(psi.mat, psi.info.map(locateQR))
  }

  val extractQRStep: ProcessingStep[Unit,Option[String]] = locateQRStep.extend("Extracción del código QR") { psi: ProcessingInfo[Option[MatOfPoint]] =>
    psi.info match {
      case Some(rect) =>
        val dstPoints = new MatOfPoint( (0.0, 0.0), (150.0,0.0), (150.0,150.0), (0.0,150.0)  )

        val h = findHomography(rect,dstPoints)
        val qr = warpImage()(psi.mat, h, new Size(150, 150))
        val qrData = QRScanner.decode(qr)
        ProcessingInfo(qr, qrData )

      case None =>
        ProcessingInfo(null, None)
    }

  }



  private val defaultQuestions = 50

  def answerMatrixStep(questions: Int = defaultQuestions): ProcessingStep[Unit, Unit] = {
    answerMatrixLocationStep.extend("Extracción de la tabla de respuestas") {
      psi: ProcessingInfo[Option[MatOfPoint]] =>
        psi.info match {
          case Some(rect) =>
            val dstPoints = AnswerMatrixMeasures.destinationContour(questions)

            val h = findHomography(rect,dstPoints)
            ProcessingInfo(warpImage()(psi.mat, h, AnswerMatrixMeasures.destinationSize(questions)), Unit)

          case None =>
            ProcessingInfo(null, Unit)
        }
    }
  }

  def cellsOfAnswerMatrix(questions: Int = defaultQuestions) = {
    answerMatrixStep(questions).extend("Localización de celdas") {
      psi: ProcessingInfo[Unit] =>
        ProcessingInfo(psi.mat, AnswerMatrixMeasures.cells(questions))
    }
  }

}

