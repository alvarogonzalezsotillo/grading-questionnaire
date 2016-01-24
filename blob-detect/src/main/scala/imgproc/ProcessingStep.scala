package imgproc


import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import common.{Sounds, BinaryConverter}
import org.opencv.core._
import org.opencv.highgui.Highgui

import org.opencv.imgproc.Imgproc

import scala.util.Try


/**
 * Created by alvaro on 13/11/15.
 */


trait ProcessingStep {

  import imgproc.ProcessingStep._

  val process: Info => Info
  val stepName: String


  def extend[T](name: String)(p: Info => Info): ProcessingStep = ExtendedStep(this, name, p)

  def withDrawContours(extractContours: Info => Option[Seq[MatOfPoint]]): ProcessingStep = extend(stepName) { p =>

    val matWithContours = for (m <- p.mat; contours <- extractContours(p)) yield {
      val newM = m.clone
      ImageProcessing.drawContours(newM, contours)
    }
    p.copy(mat = matWithContours)

  }

  def withDrawString(extractString: Info => Option[String]): ProcessingStep = extend(stepName) { p =>

    val matWithString = for (m <- p.mat; s <- extractString(p)) yield {
      val newM = m.clone()
      ImageProcessing.drawString(newM, s, new Point(10, 10))
    }
    p.copy(mat = matWithString)
  }
}


object ProcessingStep {

  import imgproc.Implicits._

  import imgproc.ImageProcessing._


  private case class Step(override val stepName: String)(override val process: Info => Info) extends ProcessingStep

  private case class ExtendedStep(previous: ProcessingStep, override val stepName: String, extendedProcess: Info => Info) extends ProcessingStep {
    override val process = (src: Info) => extendedProcess(previous.process(src))
  }

  private case class InitialStep(override val stepName: String) extends ProcessingStep {
    override val process = (m: Info) => m
  }


  implicit class AcceptStep(step: ProcessingStep) {

    private val defaultDelay = 2500

    def withFilter(name: String = "Filtrado:" + step.stepName, delay: Int = defaultDelay)(accept: Info => Boolean): ProcessingStep = {
      var lastAccept = System.currentTimeMillis()
      var lastInfo: Info = Info(None)
      step.extend(name) { psi =>
        if (System.currentTimeMillis() > lastAccept + delay && accept(psi)) {
          lastAccept = System.currentTimeMillis()
          lastInfo = psi
        }
        lastInfo
      }
    }

    def withSaveMatrix(name: String = "Grabando:" + step.stepName): ProcessingStep = {
      var lastInfo: Info = Info(None)
      step.extend(name) { psi =>
        if (!(psi eq lastInfo)) {
          lastInfo = psi
          lastInfo.mat.map(saveMatrix)
          Sounds.beep()
        }
        psi
      }
    }

  }

  def saveMatrix(m: Mat) {
    val shortDateFormat = new SimpleDateFormat("yyyyMMdd")
    val longDateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val date = new Date
    val lDate = longDateFormat.format(date)
    val sDate = shortDateFormat.format(date)
    new File(sDate).mkdirs
    val file = s"$sDate/$lDate.png"
    Highgui.imwrite(file, m)
  }


  def locateQR(answerMatrixLocation: MatOfPoint): MatOfPoint = {
    val points = answerMatrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val xAxis = (tr - tl)
    val yAxis = new Point(-xAxis.y, xAxis.x)

    val topLeft = tl -
      (yAxis * AnswerMatrixMeasures.matrixWithToTopOfQRRatio) -
      (xAxis * AnswerMatrixMeasures.matrixWithToLeftOfQRRatio)
    val topRight = topLeft + (xAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomLeft = topLeft + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)
    val bottomRight = topRight + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio)

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

    Try(doIt).filter(checkIt).toOption
  }


  trait OriginalMatInfo {
    val originalMat: Mat
    val thresholdMat: Mat
    val cleanedMat: Mat
  }

  trait LocationInfo {
    val location: Option[MatOfPoint]
    val locatedMat: Option[Mat]
  }

  trait ContoursInfo {
    val contours: Seq[MatOfPoint]
    val quadrilaterals: Seq[MatOfPoint]
    val biggestQuadrilaterals: Seq[MatOfPoint]
  }

  trait QRLocationInfo {
    val qrLocation: Option[MatOfPoint]
    val qrLocatedMat: Option[Mat]
  }

  trait QRInfo {
    val qrText: Option[String]
  }

  trait AnswersInfo {
    val answers: Option[Seq[Int]]
    val cells: Option[Seq[MatOfPoint]]
  }

  trait StudentInfo{
    val studentInfoLocation : Option[MatOfPoint]
    val studentInfoMat : Option[Mat]
  }

  case class Info(mat: Option[Mat], originalMat: Mat = null, cleanedMat: Mat = null, thresholdMat: Mat = null, contours: Seq[MatOfPoint] = null,
                  quadrilaterals: Seq[MatOfPoint] = null, biggestQuadrilaterals: Seq[MatOfPoint] = null,
                  location: Option[MatOfPoint] = None, locatedMat: Option[Mat] = None, qrLocatedMat: Option[Mat] = None, qrLocation: Option[MatOfPoint] = None,
                  qrText: Option[String] = None, answers: Option[Seq[Int]] = None, cells: Option[Seq[MatOfPoint]] = None, studentInfoLocation: Option[MatOfPoint] = None,
                   studentInfoMat: Option[Mat] = None)
    extends OriginalMatInfo with LocationInfo with ContoursInfo with QRLocationInfo with QRInfo with AnswersInfo with StudentInfo{
  }

  implicit def infoFromMat(m: Mat) = Info(Some(m), m)


  val initialStep: ProcessingStep = InitialStep("Imagen original")

  val thresholdStep = initialStep.extend("Umbral adaptativo") { psi =>
    val t = threshold()(psi.originalMat)
    psi.copy(thresholdMat = t, mat = Some(t))
  }

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)") { psi =>
    val cleaned = clean()()(psi.thresholdMat)
    psi.copy(cleanedMat = cleaned, mat = Some(cleaned))
  }

  val contourStep = noiseReductionStep.extend("Búsqueda de contornos") { psi: Info =>
    val contours = findContours(psi.cleanedMat)
    psi.copy(contours = contours, mat = Some(psi.originalMat))
  }

  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros") { csi =>
    val newContours = approximateContoursToQuadrilaterals()(csi.contours)
    csi.copy(quadrilaterals = newContours)
  }

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros") { csi =>
    val quadrilaterals = findBiggestAlignedQuadrilaterals()(csi.quadrilaterals)
    csi.copy(biggestQuadrilaterals = quadrilaterals)
  }

  val answerMatrixLocationStep = biggestQuadrilateralsStep.extend("Localización de la tabla de respuestas") { lsi =>
    val location = locateAnswerMatrix()(lsi.biggestQuadrilaterals)
    lsi.copy(location = location)
  }


  val locateQRStep = answerMatrixLocationStep.extend("Localización del código QR") { psi =>
    psi.copy(qrLocation = psi.location.map(locateQR))
  }

  val extractQRStep = locateQRStep.extend("Extracción del código QR") { psi =>

    def compute(rect: MatOfPoint) = {
      val dstPoints = new MatOfPoint((0.0, 0.0), (150.0, 0.0), (150.0, 150.0), (0.0, 150.0))

      val h = findHomography(rect, dstPoints)
      warpImage()(psi.originalMat, h, new Size(150, 150))
    }

    val m: Option[Mat] = psi.qrLocation.map(compute)
    psi.copy(mat = m, qrLocatedMat = m)
  }


  val decodeQRStep = extractQRStep.extend("Decodificación del código QR") { psi =>
    psi.copy(qrText = psi.qrLocatedMat.flatMap(QRScanner.decode))
  }

  val informationOfQRStep = decodeQRStep.extend("Información del código QR") { psi =>

    def compute(s: String) = {
      val data = BinaryConverter.fromBase64(s)
      BinaryConverter.fromBinarySolutions(data)
    }

    psi.copy(answers = psi.qrText.map(compute))
  }


  val answerMatrixStep = informationOfQRStep.extend("Extracción de la tabla de respuestas") { psi =>

    val am = for (rect <- psi.location; answers <- psi.answers) yield {

      val dstPoints = AnswerMatrixMeasures.destinationContour(answers.size)

      val h = findHomography(rect, dstPoints)
      warpImage()(psi.originalMat, h, AnswerMatrixMeasures.destinationSize(answers.size))
    }

    psi.copy(mat = am, locatedMat = am)

  }

  val studentInfoStep = answerMatrixStep.extend( "Información del alumno") { psi =>

    def studentInfoRect( qrLocation: MatOfPoint, matrixLocation: MatOfPoint ) : MatOfPoint = {
      ???
    }

    val sm: Option[(Some[MatOfPoint], Mat)] = for( qrLocation <- psi.qrLocation ; matrixLocation <- psi.location ; answers <- psi.answers ) yield {
      val rect = studentInfoRect(qrLocation,matrixLocation)
      val dstPoints = AnswerMatrixMeasures.studentInfoDestinationContour(answers.size)
      val h = findHomography(rect,dstPoints)
      ( Some(rect), warpImage()(psi.originalMat, h, AnswerMatrixMeasures.destinationSize(answers.size)) )
    }

    sm match{
      case Some((sil, sim)) =>
        psi.copy( mat = Some(sim), studentInfoMat = Some(sim), studentInfoLocation = sil )
      case None => psi
    }

  }

  val cellsOfAnswerMatrix = answerMatrixStep.extend("Localización de celdas") { psi =>
    psi.copy(cells = psi.answers.map(a => AnswerMatrixMeasures.cells(a.size)))
  }


}

