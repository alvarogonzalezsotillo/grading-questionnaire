package imgproc.steps

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import common.{BinaryConverter, Sounds}
import imgproc.steps.ProcessingStep.ExtendedStep
import imgproc.{AnswerMatrixMeasures, ImageProcessing, QRScanner}
import org.opencv.core._
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc

import scala.util.Try


/**
 * Created by alvaro on 13/11/15.
 */


trait ProcessingStep {

  import common.HMap

  type Info = HMap

  def process( implicit i: Info ) : Info
  val stepName: String

  import initialStep._


  def extend[T](name: String)(p: Info => Info): ProcessingStep = ExtendedStep(this, name, p)

  def withDrawContours(extractContours: Info => Option[Seq[MatOfPoint]]): ProcessingStep = extend(stepName) { p =>

    val matWithContours = for (m <- p(mat); contours <- extractContours(p)) yield {
      val newM = m.clone
      ImageProcessing.drawContours(newM, contours)
    }
    (mat <-- matWithContours)(p)
  }

  def withDrawString(extractString: Info => Option[String]): ProcessingStep = extend(stepName) { p =>

    val matWithString = for (m <- p(mat); s <- extractString(p)) yield {
      val newM = m.clone()
      ImageProcessing.drawString(newM, s, new Point(10, 10))
    }
    (mat <-- matWithString)(p)
  }
}


object ProcessingStep {

  import imgproc.ImageProcessing._
  import imgproc.Implicits._


  private case class Step(override val stepName: String)(override val process: Info => Info) extends ProcessingStep

  private case class ExtendedStep(previous: ProcessingStep, override val stepName: String, extendedProcess: Info => Info) extends ProcessingStep {
    def process(implicit src: Info) = extendedProcess(previous.process(src))
  }


  object Implicits {

    import OriginalMatInfo._


    implicit class AcceptStep(step: ProcessingStep) {

      private val defaultDelay = 2500

      def withFilter(name: String = "Filtrado:" + step.stepName, delay: Int = defaultDelay)(accept: Info => Boolean): ProcessingStep = {
        var lastAccept = System.currentTimeMillis()
        var lastInfo: Info = HMap()
        step.extend(name) { psi =>
          if (System.currentTimeMillis() > lastAccept + delay && accept(psi)) {
            lastAccept = System.currentTimeMillis()
            lastInfo = psi
          }
          lastInfo
        }
      }

      def withSaveMatrix(name: String = "Grabando:" + step.stepName): ProcessingStep = {
        var lastInfo: Info = HMap()
        step.extend(name) { psi =>
          if (!(psi eq lastInfo)) {
            lastInfo = psi
            lastInfo(mat).map(saveMatrix(_))
            saveMatrix(lastInfo.originalMat, "-original")
            Sounds.beep()
          }
          psi
        }
      }

    }

  }

  private def saveMatrix(m: Mat, suffix: String = "") {
    val shortDateFormat = new SimpleDateFormat("yyyyMMdd")
    val longDateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss-SSS")
    val date = new Date
    val lDate = longDateFormat.format(date)
    val sDate = shortDateFormat.format(date)
    new File(sDate).mkdirs
    val file = s"$sDate/$lDate$suffix.png"
    Highgui.imwrite(file, m)
  }


  private def locateQR(answerMatrixLocation: MatOfPoint): MatOfPoint = {
    val points = answerMatrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val xAxis = (tr - tl)
    val yAxis = new Point(-xAxis.y, xAxis.x)

    val topLeft = tl -
      (yAxis * AnswerMatrixMeasures(1).matrixWithToTopOfQRRatio) -
      (xAxis * AnswerMatrixMeasures(1).matrixWithToLeftOfQRRatio)
    val topRight = topLeft + (xAxis * AnswerMatrixMeasures(1).matrixWithToQRWidthRatio)
    val bottomLeft = topLeft + (yAxis * AnswerMatrixMeasures(1).matrixWithToQRWidthRatio)
    val bottomRight = topRight + (yAxis * AnswerMatrixMeasures(1).matrixWithToQRWidthRatio)

    new MatOfPoint(topLeft, topRight, bottomRight, bottomLeft)
  }


  def findBiggestAlignedQuadrilaterals(number: Int = 5)(contours: Seq[MatOfPoint]): Option[IndexedSeq[MatOfPoint]] = {
    val ordered = contours.sortBy(_.area).reverse

    def similarQuadrilaterals(quad: MatOfPoint) =  {
      implicit val epsilon = Epsilon(quad.area*0.25)
      contours.filter(_.area ~= quad.area )
    }

    if(false){
      println("Similar quadrilaterals:")
      println(" area:" + ordered.map(_.area).mkString(", "))
    }

    val ret = ordered.view.map(similarQuadrilaterals).filter(_.size==number).headOption
    ret.map( _.sortBy( _.boundingBox.minX).toIndexedSeq )
  }


  private def locateAnswerMatrix(imageWidth: Int, imageHeight: Int, number: Int = 5, insideLimit: Int = -8)(contours: Seq[MatOfPoint]): Option[MatOfPoint] = {
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

      val lowerExtension = (lowerRight - lowerLeft) * AnswerMatrixMeasures(1).extensionFactor
      val upperExtension = (upperRight - upperLeft) * AnswerMatrixMeasures(1).extensionFactor

      new MatOfPoint(
        upperLeft + center,
        upperRight + center + upperExtension,
        lowerRight + center + lowerExtension,
        lowerLeft + center
      )
    }

    def checkIt(contour: MatOfPoint) = {

      def insideImage(p: Point) = {
        p.x > 0 && p.y > 0 && p.x < imageWidth && p.y < imageHeight
      }

      val contour2f = new MatOfPoint2f()
      contour.convertTo(contour2f, CvType.CV_32FC2)

      def insideContour(p: Point) = {
        val inside = Imgproc.pointPolygonTest(contour2f, p, true)
        inside > insideLimit
      }

      val upperLeft = contour.toArray()(0)
      val upperRight = contour.toArray()(1)
      val w = upperRight.x - upperLeft.x

      val contoursSizeOK = contours.size == number
      val insideImageOK = contour.toArray.forall(insideImage)
      val insideContourOK = allPoints.forall(insideContour)
      val widthOK = w > imageWidth * 0.60

      val ret = contoursSizeOK && insideImageOK && insideContourOK && widthOK

      if(false){
        println(s"$contoursSizeOK  $insideImageOK $insideContourOK $widthOK $w $imageWidth")
      }

      ret
    }

    Try(doIt).filter(checkIt).toOption
  }


  object mat extends HKey[Mat]
  object originalMat  extends HKey[Mat]
  implicit def infoFromMat(m: Mat) : Info = (mat <-- m)(originalMat <-- m)(HMap())

  object initialStep extends ProcessingStep {
    override def process(implicit m: Info) = m
    override val stepName = "Imagen original"
  }

  val thresholdStep = initialStep.extend("Umbral adaptativo") { implicit psi =>
    import GrayscaleInfo._
    val t = threshold()(originalMat())
    (thresholdMat <-- t)(mat <-- t)
  }

  val noiseReductionStep = thresholdStep.extend("Eliminación de ruido (open-close)") { implicit psi =>
    import GrayscaleInfo._
    val cleaned = clean()()(thresholdMat())
    (cleanedMat <-- cleaned)(mat <-- cleaned)
  }

  val contourStep = noiseReductionStep.extend("Búsqueda de contornos") { implicit psi =>
    import GrayscaleInfo._
    import ContoursInfo._
    val c = findContours(cleanedMat())
    (contours <-- c)(mat <-- originalMat())
  }

  val quadrilateralStep = contourStep.extend("Filtro de contronos no cuadriláteros") { implicit csi =>
    import ContoursInfo._
    val newContours = approximateContoursToQuadrilaterals()(contours())
    (quadrilaterals <-- newContours)
  }

  val biggestQuadrilateralsStep = quadrilateralStep.extend("Los mayores cinco cuadriláteros") { implicit csi =>
    import ContoursInfo._
    val quadrilaterals = findBiggestAlignedQuadrilaterals()(quadrilaterals())
    (biggestQuadrilaterals <-- quadrilaterals)
  }

  val answerMatrixLocationStep = biggestQuadrilateralsStep.extend("Localización de la tabla de respuestas") { implicit lsi =>
    import LocationInfo._
    import ContoursInfo._
    val l = lsi(biggestQuadrilaterals).flatMap { biggestQuadrilaterals =>
      locateAnswerMatrix(originalMat().width(), originalMat().height())(biggestQuadrilaterals)
    }
    (location <-- l)
  }


  val locateQRStep = answerMatrixLocationStep.extend("Localización del código QR") { implicit psi =>
    import LocationInfo._
    import QRInfo._
    (qrLocation <-- psi(location).map(locateQR))
  }

  val extractQRStep = locateQRStep.extend("Extracción del código QR") { implicit psi =>
    import QRInfo._
    def compute(rect: MatOfPoint) = {
      val dstPoints = new MatOfPoint((0.0, 0.0), (150.0, 0.0), (150.0, 150.0), (0.0, 150.0))

      val h = findHomography(rect, dstPoints)
      warpImage()(originalMat(), h, new Size(150, 150))
    }

    val m = psi(qrLocation).map(compute)
    (mat <-- m)(qrLocatedMat <-- m)
  }


  val decodeQRStep = extractQRStep.extend("Decodificación del código QR") { implicit psi =>
    import QRInfo._
    (qrText <-- psi(qrLocatedMat).flatMap(QRScanner.decode))
  }

  val informationOfQRStep = decodeQRStep.extend("Información del código QR") { implicit psi =>
    import AnswersInfo._
    import QRInfo._
    def compute(s: String) = {
      val data = BinaryConverter.fromBase64(s)
      BinaryConverter.fromBinarySolutions(data)
    }
    (answers <-- psi(qrText).map(compute))
  }


  val answerMatrixStep = informationOfQRStep.extend("Extracción de la tabla de respuestas") { implicit psi =>

    val loc = for (rect <- psi(location); answers <- psi(answers) ; biggestQuadrilaterals <- psi(biggestQuadrilaterals)) yield {

      val dstPoints = AnswerMatrixMeasures(1).destinationContour(answers.size)

      val h = findHomography(rect, dstPoints)
      val locatedMat = warpImage()(psi.originalMat, h, AnswerMatrixMeasures(1).destinationSize(answers.size))
      val locatedCellHeaders = warpContours(biggestQuadrilaterals,h)

      (locatedMat,locatedCellHeaders)
    }

    val am = loc.map( _._1 )
    val lch = loc.map( _._2 )

    (mat <-- am)( locatedMat <-- am)(locatedCellHeaders <-- lch)
  }

  val studentInfoStep = answerMatrixStep.extend("Información del alumno") { psi =>


    def studentInfoRect(qrLocation: MatOfPoint, matrixLocation: MatOfPoint): MatOfPoint = {
      AnswerMatrixMeasures(1).fromMatrixToStudentInfoLocation(matrixLocation)
    }

    val sm: Option[(Some[MatOfPoint], Mat)] = for (qrLocation <- psi.qrLocation; matrixLocation <- psi.location; answers <- psi.answers) yield {
      val rect = studentInfoRect(qrLocation, matrixLocation)
      val dstPoints = AnswerMatrixMeasures(1).studentInfoDestinationContour(answers.size)
      val h = findHomography(rect, dstPoints)
      (Some(rect), warpImage()(psi.originalMat, h, AnswerMatrixMeasures(1).studentInfoDestinationSize(answers.size)))
    }

    sm match {
      case Some((sil, sim)) =>
        psi.copy(mat = Some(sim), studentInfoMat = Some(sim), studentInfoLocation = sil)
      case None => psi
    }

  }


  val cellsOfAnswerMatrix = answerMatrixStep.extend("Localización de celdas") { psi =>
    val ret = for (m <- psi.locatedMat; a <- psi.answers) yield {
      val cr = AnswerMatrixMeasures(1).cells(a.size)
      val c = for (r <- cr) yield submatrix(m, r, AnswerMatrixMeasures(1).cellWidth, AnswerMatrixMeasures(1).cellHeight)
      psi.copy(cellsRect = Some(cr), cells = Some(c))
    }
    ret.getOrElse(psi)
  }



}

